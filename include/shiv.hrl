-ifndef(shiv_hrl).
-define(shiv_hrl, true).

-define(RELIC_PLUGIN_VERSION, <<"1.0.0">>).
-define(RELIC_APPLICATION_NAME, <<"Folsom Shiv">>).

-record(shiv_metric_name, {
    category :: binary(),
    label :: binary() | list(binary()),
    units :: binary()
}).

-type shiv_metric() ::
    {histogram, #shiv_metric_name{}}
    | {histogram, #shiv_metric_name{}, uniform, integer()}
    | {histogram, #shiv_metric_name{}, exdec, integer(), integer()}
    | {histogram, #shiv_metric_name{}, slide, integer()}
    | {histogram, #shiv_metric_name{}, slide_uniform, {integer(), integer()}}
    | {gauge, #shiv_metric_name{}}
    | {counter, #shiv_metric_name{}}
    | {spiral, #shiv_metric_name{}}.

%%
%%  LIBRATO
%%

-record(librato_config, {
    user_name :: binary(),
    api_token :: binary(),
    source :: is_list(binary())
}).


%%
%%  NEW RELIC
%%

-record(relic_config, {
    guid,
    entity_name,
    license,
    use_compression
}).


-record(histogram_sample, {
    count,          % the number of things being measured (required)
    total,          % the total value measured across all things being counted (required)
    min,            % the min of values measured when count > 1 (required)
    max,            % the max of values measured when count > 1 (required)
    sum_of_squares  % sum of the squares of each measured val (optional - only needed to calc std dev.)
}).

-type client_metric_num() :: integer() | float().
-type client_metric_value() ::
    {gauge, client_metric_num()}
    | {counter, client_metric_num()}
    | #histogram_sample{}.
-type client_metric() :: {#shiv_metric_name{}, client_metric_value()}.



-endif.