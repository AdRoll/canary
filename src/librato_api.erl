%% Copyright
-module(librato_api).
-author("jesse").

%% API
-export([send_metrics/3]).

-include("shiv.hrl").

-define(LIBRATO_METRICS_POST_ENDPOINT, "metrics-api.librato.com/v1/metrics").
-define(LIBRATO_METRICS_POST_TRIES, 3).

%%
%%  Contains functions for posting metrics to NewRelic via their REST api.
%%


%% @doc Posts shiv metrics via librato API call
send_metrics(#librato_config{user_name = UserName, api_token = APIToken}, HostName, Metrics) ->
    post_metrics_report(UserName, APIToken, HostName, Metrics).


post_metrics_report(UserName, APIToken, Host, Metrics) ->
    JsonBody = {struct,
        [
            {source, Host}
            | to_metrics_json(Metrics)
        ]
    },

    post_metrics_report__(
        % NOTE: librato authenticates via https - UserName MUST be url encoded!
        <<"https://", UserName/binary, ":", APIToken/binary, "@", ?LIBRATO_METRICS_POST_ENDPOINT>>,
        iolist_to_binary(terlbox:tojson(JsonBody)),
        0
    ).


post_metrics_report__(_Url, _Body, Tries)
    when Tries >= ?LIBRATO_METRICS_POST_TRIES
    ->
    lager:error("Librato metrics post completely failed"),
    error;
post_metrics_report__(Url, Body, Tries) ->
    case catch(
        httpc:request(
            post,
            {Url, [], "application/json", Body},
            [{timeout, 3000}, {connect_timeout, 3000}],
            []
        )
    ) of
        {ok, {{_, 200, _}, _Headers, _ResponseBody}} ->
            ok;
        E ->
            lager:error("Error while posting new relic metrics", [E]),
            post_metrics_report__(Url, Body, Tries+1)
    end.


%% @doc Converts metrics into the a struct compatible with librato API
to_metrics_json(Metrics) ->
    [
        % NOTE: librato API expects both histograms and gauges as 'gauges'
        {gauges,
            to_metrics_json(
                gauge,
                [ Gauge || Gauge = {gauge, _} <- Metrics ],
                to_metrics_json(
                    histogram,
                    [ Histogram || Histogram = {histogram, _, _, _, _} <- Metrics ]
                )
            )
        },
        {counters, to_metrics_json(gauge, [ Counter || Counter = {counter, _} <- Metrics ], [])}
    ].


to_metrics_json(_Type, [], Acc) ->
    Acc;
to_metrics_json(Type, [Metric | RestGauges], Acc) ->
    to_metrics_json(Type, RestGauges,
        [
            to_metric_json(Type, Metric) | Acc
        ]
    ).

to_metric_json(Type, {MetricName, MetricValue}) ->
    {
        to_librato_name(MetricName),
        to_metric_value_json(Type, MetricValue)
    }.


to_metric_value_json(gauge, {gauge, Value}) ->
    [{value, Value}];
to_metric_value_json(counter, {counter, Value}) ->
    [{value, Value}];
to_metric_value_json(histogram, {histogram, Count, Sum, Max, Min}) ->
    [
        {count, Count},
        {sum, Sum},
        {max, Max},
        {min, Min}
    ].


to_librato_name(#shiv_metric_name{category = Category, label = Label}) ->
    terlbox:bjoin([Category, to_metrics_label_str(Label)], <<":">>).

to_metrics_label_str(Label) when is_binary(Label) ->
    Label;
to_metrics_label_str(Label) when is_list(Label) ->
    terlbox:bjoin(Label, <<":">>).