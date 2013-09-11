%% Copyright
-module(canary_app).
-author("james").

-behaviour(application).

% application
-export([start/2, stop/1]).

% application callbacks
start(_Type, _Args) ->
    canary_sup:start_link().

stop(_State) ->
    ok.
