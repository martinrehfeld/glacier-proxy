-module(gs_uploader).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {cmd, port}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(Cmd) ->
    gen_server:start(?MODULE, [Cmd], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Cmd]) ->
    Port = open_port({spawn, Cmd}, [stream, exit_status, use_stdio, in, binary]),
    {ok, #state{cmd = Cmd, port = Port}}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    Size = size(Data),
    Truncated = if
                    Size >  80 -> <<" (truncated)">>;
                    Size =< 80 -> <<>>
                end,
    error_logger:info_msg("gs_uploader: port cmd sent ~p bytes of data:~n~p~s~n",
                          [Size, binary:part(Data, 0, min(Size, 80)), Truncated]),
    {noreply, State};
handle_info({Port, {exit_status, 0}}, #state{port = Port} = State) ->
    {stop, normal, State};
handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    {stop, {port_error, Status}, State};
handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_terminated, Reason}, State};
handle_info(Info, State) ->
    error_logger:info_msg("gs_uploader: ignored unhandled message ~p~n", [Info]),
    {noreply, State}.

terminate(normal, _State) ->
    error_logger:info_msg("gs_uploader: port cmd completed successfully~n", []),
    ok;
terminate({port_terminated, Reason}, _State) ->
    error_logger:info_msg("gs_uploader: port terminated with posixcode ~p~n", [Reason]),
    ok;
terminate({port_error, Status}, _State) ->
    error_logger:info_msg("gs_uploader: port cmd failed with status ~p~n", [Status]),
    ok;
terminate(_Reason, #state{port = Port}) ->
    port_close(Port).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

