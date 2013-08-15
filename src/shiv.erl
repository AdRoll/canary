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
-export([start/2, start/3, stop/0, ping/0, track_metric/1, notify_metric/2, send_new_relic_metrics/0]).

-include("shiv.hrl").

-record(server_state, {
    host_name,
    tracked_metrics,
    new_relic_license,
    new_relic_report_pc   % TRef representing periodic call to upload metrics via new_relic_api
}).


start(HostName, NewRelicLicense) ->
    start(HostName, NewRelicLicense, []).

start(HostName, NewRelicLicense, InitialShivMetrics) ->
    lager:info("Starting shiv..."),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [HostName, NewRelicLicense, InitialShivMetrics], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init([HostName, NewRelicLicense, ShivMetrics]) ->
    process_flag(trap_exit, true),
    lager:info("Initializing shiv..."),

    % start folsom metrics application, if it's not already started
    application:start(folsom),

    % report call counts and blocks every minute to cloud watch.
    NewRelicReportPC = timer:apply_interval(6000, shiv, send_new_relic_metrics, []),

    % initialize all metrics
    init_metrics(ShivMetrics),

    State = #server_state{
            host_name = HostName,
            tracked_metrics = ShivMetrics,
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
send_new_relic_metrics() ->
    FolsomMetrics = case catch(folsom_metrics:get_metrics_value(shiv)) of
        Metrics when is_list(Metrics) ->
            Metrics;
        E ->
            lager:error("Caught error pulling folsom metrics: ~p", [E]),
            []
    end,

    RelicMetrics = build_relic_metrics(FolsomMetrics, []),

    lager:error("Relic metrics: ~p", [RelicMetrics]),
    gen_server:cast(?MODULE, {send_new_relic_metrics, RelicMetrics}).


%%
%% @doc Allows for the tracking of some additional metric not originally posted as
%%  part of the server initialization.
%%
track_metric(ShivMetric) ->
    init_metric(ShivMetric),
    gen_server:cast(?MODULE, {track_metric, ShivMetric}).


%%
%%  EXTERNAL API
%%


%%
%% @doc Notifies underlying folsom metric of some event.
%%
notify_metric(RelicMetricName, Value) ->
    folsom_metrics:notify(
        {to_folsom_name(RelicMetricName), Value}
    ).


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

new_folsom_metric({gauge, MetricName}) ->
    folsom_metrics:new_gauge(to_folsom_name(MetricName));
new_folsom_metric({counter, MetricName}) ->
    folsom_metrics:new_counter(to_folsom_name(MetricName));
new_folsom_metric({spiral, MetricName}) ->
    folsom_metrics:new_spiral(to_folsom_name(MetricName)).


folsom_metric_name({_MetricType, RelicMetricName}) ->
    to_folsom_name(RelicMetricName).


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
    SpiralValue.

%%
%%  SERVER CALL HANDLERS
%%

handle_call({ping}, _From, State) ->
    {reply, pong, State}.


handle_cast({track_metric, ShivMetric},
    State = #server_state{tracked_metrics = TrackedMetrics})
    ->
    {noreply, State#server_state{tracked_metrics = [ShivMetric | TrackedMetrics]}};
handle_cast({send_new_relic_metrics, RelicMetrics},
    State = #server_state{host_name = HostName, new_relic_license = NRLicense})
    ->
    new_relic_api:send_metrics(HostName, NRLicense, RelicMetrics),
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