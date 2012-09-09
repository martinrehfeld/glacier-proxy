-module(glacier_proxy).

-behaviour(application).

-define(ANYHOST, '_').
-define(ANYPATH, '_').


%% API
-export([start/0, stop/0]).

%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% API Function Definitions
%% ===================================================================

start() -> application:start(?MODULE).
stop()  -> application:stop(?MODULE).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = lager:start(),
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(lhttpc),
    ok = application:start(cowboy),

    Dispatch = [ {?ANYHOST, [{?ANYPATH, gp_http, []}]} ],
    {ok, _} = cowboy:start_listener(http, 100,
        cowboy_tcp_transport, [{port, gp_config:port()}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ),

    glacier_proxy_sup:start_link().

stop(_State) ->
    ok.
