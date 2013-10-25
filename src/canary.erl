%% Copyright
-module(canary).
-author("jesse").

-behavior(gen_server).

%%
%%  Works in coordination with folsom_metrics, periodically dumping tracked metrics to
%%  new relic.
%%

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

%% api
-export([
    start/2, start/3, stop/0, ping/0,
    track_metric/1, notify_metric/2,
    time_call/2, time_call/4
]).


-include("canary.hrl").

-define(METRICS_REPORT_INTERVAL, 60).

-record(server_state, {
    host_name,
    tracked_metrics,
    metrics_client_config,
    client_sync_pc, % TRef representing periodic call to sync client metrics via respective API
    publish_interval :: pos_integer(),
    next_publish_time :: pos_integer()
}).

start(HostName, MetricsClientConfig) ->
    start(HostName, MetricsClientConfig, []).

start(HostName, MetricsClientConfig, InitialCanaryMetrics) ->
    start(HostName, MetricsClientConfig, InitialCanaryMetrics, ?METRICS_REPORT_INTERVAL).

start(HostName, MetricsClientConfig, InitialCanaryMetrics, PublishInterval) ->
    lager:info("Starting canary..."),
    gen_server:start_link(
        {local, ?MODULE}, ?MODULE,
        [HostName, MetricsClientConfig, InitialCanaryMetrics, PublishInterval], []
    ).

stop() ->
    gen_server:cast(?MODULE, stop).

init([HostName, MetricsClientConfig, CanaryMetrics, PublishInterval]) ->
    process_flag(trap_exit, true),
    lager:info("Initializing canary..."),

    % start folsom metrics application, if it's not already started
    application:start(folsom),

    % initialize all metrics
    init_metrics(CanaryMetrics),

    State = #server_state{
            host_name = HostName,
            tracked_metrics = CanaryMetrics,
            metrics_client_config = MetricsClientConfig,
            publish_interval = PublishInterval
    },

    lager:info("Finished initializing canary: ~p", [State]),

    {ok, heartbeat(State)}.


next_publish_time(Interval) ->
    next_publish_time(Interval, erlang:now()).

next_publish_time(Interval, Now) ->
    canary_utils:timestamp(((trunc(canary_utils:pytime(Now)) div Interval) * Interval) + Interval).

heartbeat(#server_state{publish_interval = PublishInterval} = State) ->
    NextPublishTime = next_publish_time(PublishInterval),
    Time = trunc(timer:now_diff(NextPublishTime, erlang:now()) / 1000),
    TRef = erlang:send_after(Time, canary, {heartbeat}),

    State#server_state{
        next_publish_time = NextPublishTime,
        client_sync_pc = TRef
    }.


%%
%%  SERVER CALLS API
%%

ping() ->
    gen_server:call(?MODULE, {ping}).


%%
%% @doc Allows for the tracking of some additional metric not originally posted as
%%  part of the server initialization.
%%
track_metric(RelicMetricName = #canary_metric_name{}) ->
    lager:error("Can't track a metric by name alone: ~p", [RelicMetricName]);
track_metric(CanaryMetric) ->
    init_metric(CanaryMetric),
    gen_server:cast(?MODULE, {track_metric, CanaryMetric}).


%%
%%  EXTERNAL API
%%


%%
%% @doc Notifies underlying folsom metric of some event.
%%
notify_metric(CanaryMetric, Value) ->
    erlang:spawn(
        fun() ->
            % NOTE: start tracking on demand.
            case folsom_metrics:notify({folsom_metric_name(CanaryMetric), Value}) of
                {error, _, _} ->
                    track_metric(CanaryMetric);
                ok ->
                    ok
            end
        end
    ).


%%
%% @doc Provides a simple convenience method for wrapping function around an execution time measurement (at
%%  a sampled rate).
%%
time_call(Label, CallFun)
    when is_binary(Label)
    ->
    time_call(Label, 60, {1, 100}, CallFun).

time_call(Label, Seconds, {SampleRateNumerator, SampleRateDenominator}, CallFun)
    when is_binary(Label)
    ->
    StartTime = canary_utils:pytime(os:timestamp()),

    RV = CallFun(),

    canary_utils:rate_limited_exec(
        fun() ->
            ExecTime = canary_utils:pytime(os:timestamp()) - StartTime,

            notify_metric(
                {
                    histogram,
                    #canary_metric_name{
                            category = <<"TimedCalls">>,
                            label = Label,
                            units = <<"milliseconds">>
                    },
                    slide, Seconds
                },
                ExecTime * 1000
            ),

            RV
        end,
        SampleRateNumerator,
        SampleRateDenominator
    ),

    RV.


%%
%%  HELPER FUNCTIONS
%%


%%
%% @doc Creates a backing folsom metric in the expected format,
%%  and appropriately tagged, so we can retrieve it easily.
%%
-spec init_metric(canary_metric()) -> ok.
init_metric(CanaryMetric) ->
    new_folsom_metric(CanaryMetric),
    folsom_metrics:tag_metric(
        folsom_metric_name(CanaryMetric),
        canary
    ),
    ok.

init_metrics(CanaryMetrics) ->
    [init_metric(CanaryMetric) || CanaryMetric <- CanaryMetrics].


new_folsom_metric({histogram, MetricName}) ->
    folsom_metrics:new_histogram(to_folsom_name(MetricName));
new_folsom_metric({histogram, MetricName, uniform, Size}) ->
    folsom_metrics:new_histogram(to_folsom_name(MetricName), uniform, Size);
new_folsom_metric({histogram, MetricName, exdec, Size, Alpha}) ->
    folsom_metrics:new_histogram(to_folsom_name(MetricName), exdec, Size, Alpha);
new_folsom_metric({histogram, MetricName, slide, Seconds}) ->
    folsom_metrics:new_histogram(to_folsom_name(MetricName), slide, Seconds);
new_folsom_metric({histogram, MetricName, slide_uniform, {Seconds, Size}}) ->
    folsom_metrics:new_histogram(to_folsom_name(MetricName), slide_uniform, {Seconds, Size});
new_folsom_metric({gauge, MetricName}) ->
    folsom_metrics:new_gauge(to_folsom_name(MetricName));
new_folsom_metric({counter, MetricName}) ->
    folsom_metrics:new_counter(to_folsom_name(MetricName));
new_folsom_metric({spiral, MetricName}) ->
    folsom_metrics:new_spiral(to_folsom_name(MetricName)).


folsom_metric_name(RelicMetricName = #canary_metric_name{}) ->
    to_folsom_name(RelicMetricName);
folsom_metric_name(CanaryMetric) when is_tuple(CanaryMetric) ->
    to_folsom_name(element(2, CanaryMetric)).


to_folsom_name(#canary_metric_name{category = Cat, label = Lbl, units = Units}) ->
    canary_utils:bjoin([Cat, to_folsom_label_name(Lbl), Units], <<":">>).


to_folsom_label_name(Lbl) when is_list(Lbl) ->
    canary_utils:bjoin(Lbl, <<"|">>);
to_folsom_label_name(Lbl) when is_binary(Lbl) ->
    Lbl.


to_canary_name(FolsomMetricName) ->
    [Cat, Lbl, Units] = canary_utils:bsplit(FolsomMetricName, <<":">>),
    #canary_metric_name{
            category = Cat,
            label = to_canary_label_name(Lbl),
            units = Units
    }.

to_canary_label_name(Lbl) ->
    case canary_utils:bsplit(Lbl, <<"|">>) of
        [SingleLbl] -> SingleLbl;
        Lbls -> Lbls
    end.


%%
%% METRIC CLIENT FUNCTIONS
%%

%% @doc Sends metrics report to respective client API
send_metrics_report(MetricsClientConf = #relic_config{}, HostName, FolsomMetrics, MeasureTime) ->
    new_relic_api:send_metrics(
        MetricsClientConf,
        HostName,
        build_client_metrics(FolsomMetrics, []),
        MeasureTime
    );
send_metrics_report(MetricsClientConf = #librato_config{}, HostName, FolsomMetrics, MeasureTime) ->
    librato_api:send_metrics(
        MetricsClientConf,
        HostName,
        build_client_metrics(FolsomMetrics, []),
        MeasureTime
    ).


%%
%% @doc Construct relic metrics from underlying tracked folsom
%%  metrics to forward to new relic via the api.
%%
build_client_metrics([], Acc) ->
    Acc;
build_client_metrics([FolsomMetric | Rest], Acc) ->
    build_client_metrics(
        Rest,
        case build_client_metric(FolsomMetric) of
            undefined -> Acc;
            ClientMetric -> [ClientMetric | Acc]
        end
    ).

build_client_metric({FolsomMetricName, Value}) ->
    case to_client_metric_value(Value) of
        undefined ->
            undefined;
        ClientMetricValue ->
            {to_canary_name(FolsomMetricName), ClientMetricValue}
    end.


to_client_metric_value(FolsomMetricValue)
    when is_float(FolsomMetricValue); is_integer(FolsomMetricValue)
    ->
    {gauge, FolsomMetricValue};
to_client_metric_value([{count,_}, {one,SpiralValue}]) ->
    {counter, SpiralValue};
to_client_metric_value(FolsomHistogramValues)
    when is_list(FolsomHistogramValues)
    ->
    Stats = bear:get_statistics(FolsomHistogramValues),

    Count = canary_utils:getpl(Stats, n),
    Max = canary_utils:getpl(Stats, max),
    Min = canary_utils:getpl(Stats, min),
    Mean = canary_utils:getpl(Stats, arithmetic_mean),

    case Count of
        0 ->
            undefined;
        _ ->
            #histogram_sample {
                count = Count,
                max = Max,
                min = Min,
                total = Mean * Count
            }
    end.



%%
%%  SERVER CALL HANDLERS
%%

handle_call({ping}, _From, State) ->
    {reply, pong, State}.


handle_cast({track_metric, CanaryMetric},
    State = #server_state{tracked_metrics = TrackedMetrics})
    ->

    % Don't double track.
    State2 = case lists:member(CanaryMetric, TrackedMetrics) of
        true -> State;
        false -> State#server_state{tracked_metrics = [CanaryMetric | TrackedMetrics]}
    end,

    {noreply, State2};


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({heartbeat}, State) ->

    #server_state{
        metrics_client_config = Config,
        host_name = Host,
        next_publish_time = MeasureTime

    } = State,

    erlang:spawn(
        fun() ->
            send_metrics_report(Config, Host, folsom_metrics:get_metrics_value(canary), MeasureTime)
        end
    ),

    {noreply, heartbeat(State)};
handle_info(Info, State) ->
    lager:error("Unexpected canary message received: ~p", [Info]),
    {noreply, State}.


terminate(_Reason, #server_state{client_sync_pc =PC, tracked_metrics=CanaryMetrics}) ->
    % delete all tracked metrics
    [folsom_metrics:delete_metric(folsom_metric_name(CanaryMetric)) || CanaryMetric <- CanaryMetrics],

    % cancel periodic timer on server termination
    timer:cancel(PC),

    ok.


code_change(_OldVsn, State, _Extra) ->
    State.