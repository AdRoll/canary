%% Copyright
-module(shiv).
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
    start/5, start/6, stop/0, ping/0,
    track_metric/1, notify_metric/2,
    time_call/4,
    send_new_relic_metrics/1
]).

-include("shiv.hrl").

-record(server_state, {
    host_name,
    tracked_metrics,
    new_relic_guid,
    new_relic_entity_name,
    new_relic_license,
    new_relic_report_pc   % TRef representing periodic call to upload metrics via new_relic_api
}).


start(NewRelicGuid, NewRelicEntityName, HostName, NewRelicLicense, UseCompression) ->
    start(NewRelicGuid, NewRelicEntityName, HostName, NewRelicLicense, UseCompression, []).

start(NewRelicGuid, NewRelicEntityName, HostName, NewRelicLicense, UseCompression, InitialShivMetrics) ->
    lager:info("Starting shiv..."),
    gen_server:start_link(
        {local, ?MODULE}, ?MODULE,
        [NewRelicGuid, NewRelicEntityName, HostName, NewRelicLicense, UseCompression, InitialShivMetrics], []
    ).

stop() ->
    gen_server:cast(?MODULE, stop).

init([NewRelicGuid, NewRelicEntityName, HostName, NewRelicLicense, UseCompression, ShivMetrics]) ->
    process_flag(trap_exit, true),
    lager:info("Initializing shiv..."),

    % start folsom metrics application, if it's not already started
    application:start(folsom),

    % report call counts and blocks every minute to cloud watch.
    NewRelicReportPC = timer:apply_interval(60000, shiv, send_new_relic_metrics, [UseCompression]),

    % initialize all metrics
    init_metrics(ShivMetrics),

    State = #server_state{
            host_name = HostName,
            tracked_metrics = ShivMetrics,
            new_relic_guid = NewRelicGuid,
            new_relic_entity_name = NewRelicEntityName,
            new_relic_license = NewRelicLicense,
            new_relic_report_pc = NewRelicReportPC
    },

    lager:info("Finished initializing shiv: ~p", [State]),
    {ok, State}.


%%
%%  SERVER CALLS API
%%

ping() ->
    gen_server:call(?MODULE, {ping}).

%%
%% @doc Wired up as periodic call.  Pulls all tracked metric values from folsom
%%  via reserved tag, and forwards to new relic via api.
send_new_relic_metrics(UseCompression) ->
    FolsomMetrics = case catch(folsom_metrics:get_metrics_value(shiv)) of
        Metrics when is_list(Metrics) ->
            Metrics;
        E ->
            lager:error("Caught error pulling folsom metrics: ~p", [E]),
            []
    end,

    RelicMetrics = build_relic_metrics(FolsomMetrics, []),

    gen_server:cast(?MODULE, {send_new_relic_metrics, RelicMetrics, UseCompression}).


%%
%% @doc Allows for the tracking of some additional metric not originally posted as
%%  part of the server initialization.
%%
track_metric(RelicMetricName = #relic_metric_name{}) ->
    lager:error("Can't track a metric by name alone: ~p", [RelicMetricName]);
track_metric(ShivMetric) ->
    init_metric(ShivMetric),
    gen_server:cast(?MODULE, {track_metric, ShivMetric}).


%%
%%  EXTERNAL API
%%


%%
%% @doc Notifies underlying folsom metric of some event.
%%
notify_metric(RelicMetric, Value) ->
    erlang:spawn(
        fun() ->
            % NOTE: start tracking on demand.
            case folsom_metrics:notify({folsom_metric_name(RelicMetric), Value}) of
                {error, _, _} ->
                    track_metric(RelicMetric);
                ok ->
                    ok
            end
        end
    ).


%%
%% @doc Provides a simple convenience method for wrapping function around an execution time measurement (at
%%  a sampled rate).
%%
time_call(Label, Seconds, {SampleRateNumerator, SampleRateDenominator}, CallFun)
    when is_binary(Label)
    ->
    StartTime = terlbox:pytime(os:timestamp()),

    RV = CallFun(),

    terlbox:rate_limited_exec(
        fun() ->
            ExecTime = terlbox:pytime(os:timestamp()) - StartTime,

            notify_metric(
                {
                    histogram,
                    #relic_metric_name{category = <<"TimedCalls">>, label = Label, units = <<"milliseconds">>},
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
-spec init_metric(shiv_metric()) -> ok.
init_metric(ShivMetric) ->
    new_folsom_metric(ShivMetric),
    folsom_metrics:tag_metric(
        folsom_metric_name(ShivMetric),
        shiv
    ),
    ok.

init_metrics(ShivMetrics) ->
    [init_metric(ShivMetric) || ShivMetric <- ShivMetrics].


%%
%% @doc Construct relic metrics from underlying tracked folsom
%%  metrics to forward to new relic via the api.
%%
build_relic_metrics([], Acc) ->
    Acc;
build_relic_metrics([FolsomMetric | Rest], Acc) ->
    build_relic_metrics(
        Rest, [build_relic_metric(FolsomMetric) | Acc]
    ).

build_relic_metric({FolsomMetricName, Value}) ->
    {to_relic_name(FolsomMetricName), to_relic_value(Value)}.


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


folsom_metric_name(RelicMetricName = #relic_metric_name{}) ->
    to_folsom_name(RelicMetricName);
folsom_metric_name(ShivMetric) when is_tuple(ShivMetric) ->
    to_folsom_name(element(2, ShivMetric)).


to_folsom_name(#relic_metric_name{category = Cat, label = Lbl, units = Units}) ->
    terlbox:bjoin([Cat, Lbl, Units], <<":">>).

to_relic_name(FolsomMetricName) ->
    [Cat, Lbl, Units] = terlbox:bsplit(FolsomMetricName, <<":">>),
    #relic_metric_name{category = Cat, label = Lbl, units = Units}.

to_relic_value(FolsomMetricValue)
    when is_float(FolsomMetricValue); is_integer(FolsomMetricValue)
    ->
    FolsomMetricValue;
to_relic_value([{count,_}, {one,SpiralValue}]) ->
    SpiralValue;
to_relic_value(FolsomHistogramValues)
    when is_list(FolsomHistogramValues)
    ->
    Stats = bear:get_statistics(FolsomHistogramValues),

    Count = terlbox:getpl(Stats, n),
    Max = terlbox:getpl(Stats, max),
    Min = terlbox:getpl(Stats, min),
    Mean = terlbox:getpl(Stats, arithmetic_mean),

    #relic_metric_sample{
        count = Count,
        max = Max,
        min = Min,
        total = Mean * Count
    }.

%%
%%  SERVER CALL HANDLERS
%%

handle_call({ping}, _From, State) ->
    {reply, pong, State}.


handle_cast({track_metric, ShivMetric},
    State = #server_state{tracked_metrics = TrackedMetrics})
    ->

    % Don't double track.
    State2 = case lists:member(ShivMetric, TrackedMetrics) of
        true -> State;
        false -> State#server_state{tracked_metrics = [ShivMetric | TrackedMetrics]}
    end,

    {noreply, State2};


handle_cast({send_new_relic_metrics, RelicMetrics, UseCompression},
    State = #server_state{
            host_name = HostName,
            new_relic_guid = NRGuid,
            new_relic_license = NRLicense,
            new_relic_entity_name = NREntityName
    })
    ->
    erlang:spawn(
        fun() ->
            new_relic_api:send_metrics(NRGuid, NREntityName, HostName, NRLicense, UseCompression, RelicMetrics)
        end
    ),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(Info, State) ->
    lager:error("Unexpected shiv message received: ~p", [Info]),
    {noreply, State}.


terminate(_Reason, #server_state{new_relic_report_pc=PC, tracked_metrics=ShivMetrics}) ->
    % delete all tracked metrics
    [folsom_metrics:delete_metric(folsom_metric_name(ShivMetric)) || ShivMetric <- ShivMetrics],

    % cancel periodic timer on server termination
    timer:cancel(PC),

    ok.


code_change(_OldVsn, State, _Extra) ->
    State.