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
-export([new_metric/1, send_new_relic_metrics/0]).

-include("shiv.hrl").

-record(server_state, {
    new_relic_license,
    new_relic_report_pc   % TRef representing periodic call to upload metrics via new_relic_api
}).


start(NewRelicLicense) ->
    lager:info("Starting shiv..."),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [NewRelicLicense], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init([NewRelicLicense]) ->
    process_flag(trap_exit, true),
    lager:info("Initializing shiv..."),

    % start folsom metrics application, if it's not already started
    application:start(folsom_metrics),

    % report call counts and blocks every minute to cloud watch.
    NewRelicReportPC = timer:apply_interval(60000, shiv, send_new_relic_metrics, []),

    State = #server_state{
            new_relic_license = NewRelicLicense,
            new_relic_report_pc = NewRelicReportPC
    },

    lager:info("Finished initializing shiv: ~p", [State]),
    {ok, State}.


%%
%%  EXTERNAL API
%%

send_new_relic_metrics() ->
    Metrics = build_relic_metrics(
        folsom_metrics:get_metrics_value(shiv), []
    ),

    lager:error("Metrics: ~p", [Metrics]).

%%
%% @doc Creates a backing folsom metric in the expected format,
%%  and appropriately tagged, so we can retrieve it easily.
%%
-spec new_metric(shiv_metric()) -> ok.
new_metric(ShivMetric) ->
    new_folsom_metric(ShivMetric),
    folsom_metrics:tag_metric(
        folsom_metric_name(ShivMetric),
        shiv
    ),
    ok.

%%
%%  HELPER FUNCTIONS
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
    folsom_metrics:new_gauge(folsom_metric_name(MetricName));
new_folsom_metric({counter, MetricName}) ->
    folsom_metrics:new_counter(folsom_metric_name(MetricName));
new_folsom_metric({spiral, MetricName}) ->
    folsom_metrics:new_spiral(folsom_metric_name(MetricName)).


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
to_relic_value(_) ->
    undefined.

%%
%%  SERVER CALLS API
%%

ping() ->
    gen_server:call(?MODULE, {ping}).

%%
%%  SERVER CALL HANDLERS
%%

handle_call({ping}, _From, State) ->
    {reply, pong, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    lager:error("Unexpected shiv message received: ~p", [Info]),
    {noreply, State}.

% cancel periodic timer on server termination
terminate(_Reason, #server_state{new_relic_report_pc=PC}) ->
    timer:cancel(PC),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.