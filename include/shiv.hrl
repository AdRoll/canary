-ifndef(shiv_hrl).
-define(shiv_hrl, true).

-define(RELIC_PLUGIN_VERSION, <<"1.0.0">>).
-define(RELIC_APPLICATION_NAME, <<"Folsom Shiv">>).

-record(relic_metric_name, {
    category :: binary(),
    label :: binary(),
    units :: binary()
}).

-record(relic_metric_sample, {
    count,          % the number of things being measured (required)
    total,          % the total value measured across all things being counted (required)
    min,            % the min of values measured when count > 1 (required)
    max,            % the max of values measured when count > 1 (required)
    sum_of_squares  % sum of the squares of each measured val (optional - only needed to calc std dev.)
}).

-type relic_metric_value() :: #relic_metric_sample{} | integer() | float().
-type relic_metric() :: {#relic_metric_name{}, relic_metric_value()}.


-type shiv_metric_name() :: atom().
-type shiv_metric() ::
    {gauge, #relic_metric_name{}}
    | {counter, #relic_metric_name{}}
    | {spiral, #relic_metric_name{}}.


-endif.