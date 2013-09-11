%% Copyright
-module(librato_api).
-author("jesse").

%% API
-export([send_metrics/4]).

-include("canary.hrl").

-define(LIBRATO_COUNTER, counter).
-define(LIBRATO_GAUGE, gauge).
-define(LIBRATO_METRICS_POST_ENDPOINT, "https://metrics-api.librato.com/v1/metrics").
-define(LIBRATO_METRICS_POST_TRIES, 3).
-define(SEP, <<".">>).

-record(librato_display_attributes, {
    color :: string(),
    display_max :: number(),
    display_min :: number(),
    display_units_long :: string(),
    display_units_short :: string(),
    display_stacked :: boolean(),
    display_transform :: string()
}).

-record(librato_metric, {
    name :: string(),
    period :: pos_integer(),
    description :: string(),
    display_name :: string(),
    attributes :: #librato_display_attributes{},
    measure_time :: pos_integer(),
    value :: number(),
    source :: string(),
    count :: pos_integer(),
    sum :: number(),
    max :: number(),
    min :: number(),
    sum_squares :: number(),
    summarize_function :: string(),
    aggregate :: boolean(),
    type :: gauge | counter
}).

%% API

%% @doc Post a list of metrics to Librato Metrics.
send_metrics(Config, Host, Metrics, MeasureTime) ->

    try to_librato_metrics(Metrics) of
        LibratoMetrics ->
            lager:error("LibratoMetrics: ~p", [LibratoMetrics]),

            {Gauges, Counters} = lists:partition(fun is_gauge/1, LibratoMetrics),

            lager:error("Gauges: ~p", [Gauges]),
            lager:error("Counters: ~p", [Counters]),

            send_librato_metrics(Config, Host, Gauges, Counters, MeasureTime)
    catch
        _:Reason ->
            lager:error("send_metrics error: ~p", [Reason]),
            {error, Reason}
    end.


%% INTERNAL

%% @doc Posts 
send_librato_metrics(_Config, _Host, [], [], _MeasureTime) ->
    ok;
send_librato_metrics(Config, Host, Gauges, Counters, MeasureTime) ->

    #librato_config{
        user_name = UserName,
        api_token = APIToken
    } = Config,

    Body = iolist_to_binary(canary_utils:tojson({struct, [
        {source, librato_source(Config, Host)},
        {measure_time, canary_utils:pytime(MeasureTime)},
        {gauges, lists:map(fun to_json_struct/1, Gauges)},
        {counters, lists:map(fun to_json_struct/1, Counters)}
    ]})),

    lager:error("Body: ~p", [Body]),

    post_metrics(UserName, APIToken, Body, ?LIBRATO_METRICS_POST_TRIES).

%% @doc Posts a JSONBody of metrics values to Librato.
post_metrics(_User, _Token, _Body, 0) ->
    lager:error("Metrics post failed after ~p retries.", [?LIBRATO_METRICS_POST_TRIES]),
    error;
post_metrics(User, Token, Body, Retries) ->
    Headers = [basic_auth(User, Token), {"Connection", "close"}],
    Request = {?LIBRATO_METRICS_POST_ENDPOINT, Headers, "application/json", Body},
    HttpOptions = [{timeout, 5000}, {connect_timeout, 3000}],
    Options = [{body_format, binary}, {full_result, false}],

    try httpc:request(post, Request, HttpOptions, Options) of
        {ok, {200, _}} ->
            ok;
        {ok, {StatusCode, _}} ->
            lager:error("Non 200 status code: ~p, retrying...", [StatusCode]),
            post_metrics(User, Token, Body, Retries - 1);
        {error, Reason} ->
            lager:error("HTTP error: ~p, retrying...", [Reason]),
            post_metrics(User, Token, Body, Retries - 1)
    catch
        Exception ->
            lager:error("HTTP exception: ~p, retrying...", [Exception]),
            post_metrics(User, Token, Body, Retries - 1)
    end.

%% @doc Constructs an HTTP Basic Authentication header.
basic_auth(UserName, APIToken) ->
    Encoded = base64:encode_to_string(lists:append([canary_utils:str(UserName), ":", canary_utils:str(APIToken)])),
    {"Authorization", "Basic " ++ Encoded}.

%% @doc Convert a list of Canary metrics to Librato metrics.
to_librato_metrics(List) ->
    to_librato_metrics(List, []).

%% @doc Convert individual Folsom metrics to Librato metrics.
to_librato_metrics([], A) ->
    A;
to_librato_metrics([{Name, {counter, Value}} | Rest], A) ->
    Metric = #librato_metric{name = librato_name(Name), value = Value, type = ?LIBRATO_GAUGE},
    to_librato_metrics(Rest, [Metric|A]);
to_librato_metrics([{Name, {gauge, Value}} | Rest], A) ->
    Metric = #librato_metric{name = librato_name(Name), value = Value, type = ?LIBRATO_GAUGE},
    to_librato_metrics(Rest, [Metric|A]);
to_librato_metrics([{Name, #histogram_sample{} = Value} | Rest], A) ->
    #histogram_sample{count = Count, max = Max, min = Min, total = Total} = Value,
    Metric = #librato_metric{
        name = librato_name(Name),
        count = Count,
        max = Max,
        min = Min,
        sum = Total,
        type = ?LIBRATO_GAUGE
    },
    to_librato_metrics(Rest, [Metric|A]);
to_librato_metrics([{_Name, Value} | Rest], A) ->
    lager:error("Unsupported  type: ~p", [Value]),
    to_librato_metrics(Rest, A).


%% TODO Support arbitrary Folsom metrics.
%% to_librato_metrics([{Name, {history, Value}} | Rest], A) ->
%%     Metric = #librato_metric{name = librato_name(Name), value = Value, type = ?LIBRATO_GAUGE},
%%     to_librato_metrics(Rest, [Metric|A]);
%% to_librato_metrics([{Name, {meter, Value}} | Rest], A) ->
%%     Metric = #librato_metric{name = librato_name(Name), value = Value, type = ?LIBRATO_GAUGE},
%%     to_librato_metrics(Rest, [Metric|A]);
%% to_librato_metrics([{Name, {meter_reader, Value}} | Rest], A) ->
%%     Metric = #librato_metric{name = librato_name(Name), value = Value, type = ?LIBRATO_COUNTER},
%%     to_librato_metrics(Rest, [Metric|A]);
%% to_librato_metrics([{Name, {duration, Value}} | Rest], A) ->
%%     Metric = #librato_metric{name = librato_name(Name), value = Value, type = ?LIBRATO_GAUGE},
%%     to_librato_metrics(Rest, [Metric|A]);
%% to_librato_metrics([{Name, {spiral, Value}} | Rest], A) ->
%%     Metric = #librato_metric{name = librato_name(Name), value = Value, type = ?LIBRATO_GAUGE},
%%     to_librato_metrics(Rest, [Metric|A]);


%% @doc TODO
librato_name(#canary_metric_name{category = Category, label = Label}) ->
    canary_utils:bjoin([Category, canary_utils:bjoin(Label)], ?SEP).

%% @doc TODO
librato_source(Config, Host) when not is_binary(Host) ->
    librato_source(Config, canary_utils:tobin(Host));
librato_source(#librato_config{source = Source}, Host) ->
    canary_utils:bjoin([canary_utils:bjoin(Source), Host], ?SEP).

%% @doc Converts a Librato metric record to a JSON struct.
to_json_struct(#librato_metric{} = Record) ->
    PropList = lists:filter(fun null_values/1, to_proplist(Record)),
    {struct, PropList}.

%% @doc Converts records to a proplist.
to_proplist(#librato_metric{} = Record) ->
    lists:zip(record_info(fields, librato_metric), tl(tuple_to_list(Record)));
to_proplist(#librato_display_attributes{} = Record) ->
    lists:zip(record_info(fields, librato_display_attributes), tl(tuple_to_list(Record))).

%% @doc Predicate used to partition a list of Librato metrics into gauges and counters.
is_gauge(#librato_metric{type = gauge}) ->
    true;
is_gauge(_) ->
    false.

null_values({_Key, Value}) ->
    Value =/= undefined.