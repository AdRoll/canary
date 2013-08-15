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
    ok.

new_metric(ShivMetric) ->
    new_folsom_metric(ShivMetric),
    folsom_metrics:tag_metric(metric_name(ShivMetric), shiv).

%%
%%  HELPER FUNCTIONS
%%

new_folsom_metric({gauge, MetricName}) ->
    folsom_metrics:new_gauge(MetricName);
new_folsom_metric({counter, MetricName}) ->
    folsom_metrics:new_counter(MetricName);
new_folsom_metric({spiral, MetricName}) ->
    folsom_metrics:new_spiral(MetricName).

metric_name({_MetricType, MetricName}) ->
    MetricName.

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