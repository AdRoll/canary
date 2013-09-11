%% Copyright
-module(canary_boot).
-author("james").

%% API
-export([start/0, stop/0]).

start() ->
    application:start(canary).

stop() ->
    application:stop(canary).