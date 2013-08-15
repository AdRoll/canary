%% Copyright
-module(new_relic_api).
-author("jesse").

-include("shiv.hrl").

%% API
-export([send_metric/4, send_metrics/3]).

-define(RELIC_METRICS_POST_ENDPOINT, "https://platform-api.newrelic.com/platform/v1/metrics").
-define(RELIC_METRICS_POST_TRIES, 3).

%%
%%  Contains functions for posting metrics to NewRelic via their REST api.
%%

send_metric(HostName, LicenseKey, RelicMetricName, RelicMetricValue) ->
    send_metrics(HostName, LicenseKey, [{RelicMetricName, RelicMetricValue}]).

send_metrics(HostName, LicenseKey, Metrics) ->
    post_metric_report(HostName, LicenseKey, Metrics).


%% @doc Posts specified metrics to new relic's designated endpoint for receiving
%%  such reports.
post_metric_report(HostName, LicenseKey, Metrics) ->
    BodyJson = {struct,
        [
            {agent,
                {struct,
                    [
                        {host, dru:tobin(HostName)},
                        {pid, dru:tobin(pid_to_list(self()))},
                        {version, ?RELIC_PLUGIN_VERSION}
                    ]
                }
            },
            {components,
                [
                    {struct,
                        [
                            {name, ?RELIC_APPLICATION_NAME},
                            {guid, ?RELIC_PLUGIN_GUID},
                            {duration, 60},
                            {metrics, to_metrics_json(Metrics)}
                        ]
                    }
                ]
            }
        ]
    },

    post_metric_report__(LicenseKey, iolist_to_binary(terlbox:tojson(BodyJson)), 0).


post_metric_report__(_LicenseKey, _Body, Tries)
    when Tries >= ?RELIC_METRICS_POST_TRIES
    ->
    lager:error("New relic metrics post completely failed"),
    error;
post_metric_report__(LicenseKey, Body, Tries) ->
    lager:error("Attempting to post metric report to new relic: ~p", [Body]),
    case catch(
        httpc:request(post,
            {
                ?RELIC_METRICS_POST_ENDPOINT,
                [{"X-License-Key", LicenseKey}, {"Accept", "application/json"}, {"Connection", "close"}],
                "application/json",
                Body
            },
            [{timeout, 3000}, {connect_timeout, 3000}],
            []
        )
    ) of
        {ok, {{_, 200, _}, _Headers, _ResponseBody}} ->
            ok;
        E ->
            lager:error("Error while posting new relic metrics", [E]),
            post_metric_report__(LicenseKey, Body, Tries+1)
    end.




%% @doc Converts a property list of metric names and values to correct json format
%%  expected in metrics post body
to_metrics_json(Metrics) ->
    to_metrics_json__(Metrics, {struct, []}).

to_metrics_json__([], Acc) ->
    Acc;
to_metrics_json__([Metric | RestMetrics], {struct, MetricsAcc}) ->
    to_metrics_json__(RestMetrics, {struct, [to_metric_json(Metric) | MetricsAcc]}).


to_metric_json({MetricName, MetricValue}) ->
    {to_metric_str(MetricName), to_metric_value_json(MetricValue)}.

to_metric_str(#relic_metric_name{category = Cat, label = Label, units = Units}) ->
    "Component2/" ++ Cat ++ "/" ++ Label ++ "[" ++ Units ++ "]".

to_metric_value_json(MetricValue) when is_float(MetricValue); is_integer(MetricValue) ->
    MetricValue;
to_metric_value_json(MetricSample = #relic_metric_sample{count = Count, total = Total, max = Max, min = Min}) ->
    attach_sum_of_squares(
        MetricSample,
        {struct,
            [
                {count, Count},
                {total, Total},
                {max, Max},
                {min, Min}
            ]
        }
    ).

attach_sum_of_squares(#relic_metric_sample{sum_of_squares = undefined}, MetricJson) ->
    MetricJson;
attach_sum_of_squares(#relic_metric_sample{sum_of_squares = SumOfSqrs}, {struct, MetricProps}) ->
    {struct, [{sum_of_squares, SumOfSqrs} | MetricProps]}.



