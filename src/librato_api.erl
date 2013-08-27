%% Copyright
-module(librato_api).
-author("jesse").

%% API
-export([send_metrics/3]).

-include("shiv.hrl").

-define(LIBRATO_METRICS_POST_ENDPOINT, "https://metrics-api.librato.com/v1/metrics").
-define(LIBRATO_METRICS_POST_TRIES, 3).

-define(SEP, <<".">>).


%%
%%  Contains functions for posting metrics to NewRelic via their REST api.
%%


%% @doc Posts shiv metrics via librato API call
send_metrics(#librato_config{user_name = UserName, api_token = APIToken, source = Source}, HostName, Metrics) ->
    post_metrics_report(UserName, APIToken, Source, HostName, Metrics).


post_metrics_report(UserName, APIToken, Source, Host, Metrics) ->

    % NOTE: only post if there's something to post.
    case to_metrics_json(Metrics) of
        [] ->
            lager:error("No metrics to post, skipping"),
            ok;

        MetricsJson ->

            JsonBody = {struct,
                [
                    {source, terlbox:bjoin(Source ++ [terlbox:tobin(Host)], ?SEP)}
                    | MetricsJson
                ]
            },

            lager:error("About to post json", [JsonBody]),

            post_metrics_report__(
                % NOTE: librato authenticates via https
                terlbox:str(?LIBRATO_METRICS_POST_ENDPOINT),
                post_metrics_authorization_header(UserName, APIToken),
                iolist_to_binary(terlbox:tojson(JsonBody)),
                0
            )
    end.

post_metrics_authorization_header(UserName, APIToken) ->
    Encoded = base64:encode_to_string(lists:append([terlbox:str(UserName), ":", terlbox:str(APIToken)])),
    {"Authorization", "Basic " ++ Encoded}.

post_metrics_report__(_Url, _AuthHeader, _Body, Tries)
    when Tries >= ?LIBRATO_METRICS_POST_TRIES
    ->
    lager:error("Librato metrics post completely failed"),
    error;
post_metrics_report__(Url, AuthHeader, Body, Tries) ->

    lager:error("Posting to librato API: url=~p, body=~p", [Url, Body]),

    case catch(
        httpc:request(
            post,
            {
                Url,
                [AuthHeader, {"Connection", "close"}],
                "application/json",
                Body
            },
            [{timeout, 5000}, {connect_timeout, 3000}],
            [{body_format,binary}]
        )
    ) of
        {ok, {{_, 200, _}, _Headers, _ResponseBody}} ->
            ok;
        E ->
            lager:error("Error while posting new relic metrics ~p", [E]),
            post_metrics_report__(Url, AuthHeader, Body, Tries+1)
    end.


%% @doc Converts metrics into the a struct compatible with librato API
to_metrics_json(Metrics) ->
    MetricsJson = to_gauges_json(Metrics) ++ to_counters_json(Metrics),
    MetricsJson.

to_gauges_json(Metrics) ->
    to_gauges_json__(
        % NOTE: librato API counts histograms, counters, gauges as 'gauges'
        to_metrics_json_params(
            gauge,
            [ Gauge || Gauge = {_, {gauge, _}} <- Metrics ],
            to_metrics_json_params(
                histogram,
                [ Histogram || Histogram = {_, #histogram_sample{count = Count}} <- Metrics, Count > 0 ],
                to_metrics_json_params(
                    counter,
                    [ Counter || Counter = {_, {counter, _}} <- Metrics ],
                    []
                )
            )
        )
    ).
to_gauges_json__([]) ->
    [];
to_gauges_json__(JsonParams) ->
    [{gauges, {struct, JsonParams}}].


to_counters_json(Metrics) ->
    [].
    %to_counters_json__(
    %    to_metrics_json_params(counter, [ Counter || Counter = {_, {counter, _}} <- Metrics ], [])
    %).
to_counters_json__([]) ->
    [];
to_counters_json__(JsonParams) ->
    [{counters, {struct, JsonParams}}].


to_metrics_json_params(_Type, [], Acc) ->
    Acc;
to_metrics_json_params(Type, [Metric | RestGauges], Acc) ->
    to_metrics_json_params(Type, RestGauges,
        [
            to_metric_json_params(Type, Metric) | Acc
        ]
    ).

to_metric_json_params(Type, {MetricName, MetricValue}) ->
    {
        to_librato_name(MetricName),
        {struct, to_metric_value_json_params(Type, MetricValue)}
    }.


to_metric_value_json_params(gauge, {gauge, Value}) ->
    [{value, Value}];
to_metric_value_json_params(counter, {counter, Value}) ->
    [{value, Value}];
to_metric_value_json_params(
    histogram,
    #histogram_sample{count=Count, total=Sum, max=Max, min=Min})
    ->
    [
        {count, Count},
        {sum, Sum},
        {max, Max},
        {min, Min}
    ].


to_librato_name(#shiv_metric_name{category = Category, label = Label}) ->
    terlbox:bjoin([Category, to_metrics_label_str(Label)], ?SEP).

to_metrics_label_str(Label) when is_binary(Label) ->
    Label;
to_metrics_label_str(Label) when is_list(Label) ->
    terlbox:bjoin(Label, ?SEP).