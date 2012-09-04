-module(glacier_proxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [webserver()]} }.


%% ===================================================================
%% Internal functions
%% ===================================================================

webserver() ->
    MiddlewareConfig = [{mods, [{elli_access_log, [{ident, 'glacier-proxy'},
                                                   {facility, local7}]},
                                {elli_date, []},
                                {gp_http,   []}
                               ]}
                       ],

    {webserver,
     {elli, start_link, [[{port, gp_config:port()},
                          {callback, elli_middleware},
                          {callback_args, MiddlewareConfig},
                          {name, {local, elli}}]]},
     permanent, 2000, worker, [elli]}.
