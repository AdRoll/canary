%% API

-define(RELIC_PLUGIN_VERSION, "1.0.0").
-define(RELIC_PLUGIN_GUID, "com.adroll.shiv").

-record(relic_metric_name, {
    category,
    label,
    units
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