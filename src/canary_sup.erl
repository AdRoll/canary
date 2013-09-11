%% Copyright
-module(canary_sup).
-author("james").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor
-export([init/1]).

%% API
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
init([]) ->
    lager:info("application:get_env(canary): ~p", [application:get_env(canary)]),

    CanarySpec = {canary_server, {canary_server, start, []}, permanent, 5000, worker, [canary_server]},

    lager:info("CanarySpec: ~p", [CanarySpec]),

    {ok, {{one_for_one, 5, 10}, [CanarySpec]}}.
