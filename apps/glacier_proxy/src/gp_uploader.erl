-module(gp_uploader).
-behaviour(gen_server).

-include("glacier_proxy.hrl").

%% AWS Glacier allows a maximum of 10,000 parts per upload, so the
%% PART_SIZE will determine the biggest archive size that can be
%% uploaded by this service, e.g. 128 MB part size will allow an
%% archive size of up to ~1.2 TB.
-define(PART_SIZE, (128 * 1024 * 1024)). % 128 MB

-define(CONNECT_TIMEOUT, 1000).

-record(state, {cmd, port, socket, parts}).
-record(part, {start, len, upload_state, status}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/2, info/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(Cmd, TcpPort) ->
    gen_server:start(?MODULE, [Cmd, TcpPort], []).

info(Uploader) ->
    gen_server:call(Uploader, info).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Cmd, TcpPort]) ->
    Port = open_port(
        {spawn, "sh -c '" ++ Cmd ++ "' "
                "| nc -4l localhost " ++ ?i2l(TcpPort)},
        [stream, exit_status, use_stdio, stderr_to_stdout, in, binary]),

    {ok, Socket} = connect_socket(TcpPort),

    {ok, #state{cmd = Cmd, port = Port, socket = Socket, parts = []}}.

handle_call(info, _From, State) ->
    {reply, state_to_info(State), State};
handle_call(Request, _From, State) ->
    {stop, {unhandled_call, Request}, State}.

handle_cast(Msg, State) ->
    {stop, {unhandled_cast, Msg}, State}.


handle_info({tcp, Socket, Data}, #state{socket = Socket, parts = []} = State) ->
error_logger:info_msg("Received ~p bytes of data (~p more msgs pending)~n", [byte_size(Data), process_info(self(), message_queue_len)]),
    initiate_upload(),
    inet:setopts(Socket, [{active,once}]),
    {noreply, State#state{parts = initiate_part(Data, [])}};
handle_info({tcp, Socket, Data}, #state{socket = Socket, parts = Parts} = State) ->
error_logger:info_msg("Received ~p bytes of data (~p more msgs pending)~n", [byte_size(Data), process_info(self(), message_queue_len)]),
    inet:setopts(Socket, [{active,once}]),
    {noreply, State#state{parts = do_upload(Data, Parts)}};
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    %% @todo take note that the socket was closed and wait for the port to exit
    error_logger:info_msg("TCP Socket ~p was closed~n", [Socket]),
    {noreply, State};


handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    {stop, {unexpected_data, Data}, State};
handle_info({Port, {exit_status, 0}}, #state{port = Port} = State) ->
    {stop, normal, State};
handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    {stop, {port_error, Status}, State};
handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_terminated, Reason}, State};
handle_info(Info, State) ->
    {stop, {unhandled_info, Info}, State}.


terminate(normal, _State) ->
    error_logger:info_msg("gs_uploader: port cmd completed successfully~n", []),
    finalize_upload(),
    ok;
terminate({unexpected_data, Data}, _State) ->
    error_logger:info_msg("gs_uploader: port sent unexpted data:~n~p~n", [Data]),
    abort_upload(),
    ok;
terminate({port_terminated, Reason}, _State) ->
    error_logger:info_msg("gs_uploader: port terminated with posixcode ~p~n", [Reason]),
    abort_upload(),
    ok;
terminate({port_error, Status}, _State) ->
    error_logger:info_msg("gs_uploader: port cmd failed with status ~p~n", [Status]),
    abort_upload(),
    ok;
terminate(Reason, #state{port = Port}) ->
    error_logger:info_msg("gs_uploader: terminating abnormally for ~p~n", [Reason]),
    abort_upload(),
    port_close(Port).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

initiate_upload() ->
    %% @todo issue AWS Glacier Initiate Multipart Upload
    ok.

do_upload(Data, Parts) ->
    [Current | Done] = Parts,
    DataSize = byte_size(Data),
    LenToGo = ?PART_SIZE - Current#part.len,
    if
        DataSize >= LenToGo ->
            {RemainingData, NewData} = split_data(Data, LenToGo),
            initiate_part(NewData, [finalize_part(Current, RemainingData) | Done]);

        DataSize < LenToGo ->
            [upload_data(Current, Data) | Done]
    end.

finalize_upload() ->
    %% @todo issue AWS Glacier Complete Multipart Upload
    ok.

abort_upload() ->
    %% @todo issue AWS Glacier Abort Multipart Upload if started
    ok.

initiate_part(Data, Done) when byte_size(Data) > ?PART_SIZE ->
    {Data1, Data2} = split_data(Data, ?PART_SIZE),
    initiate_part(Data2, initiate_part(Data1, Done));
initiate_part(Data, Done) when byte_size(Data) =:= ?PART_SIZE ->
    [Current | Done] = initiate_part(<<>>, Done),
    [finalize_part(Current, Data) | Done ];
initiate_part(Data, Done) ->
    Pos = ?PART_SIZE * length(Done),
    %% @todo issue AWS Glacier Upload Part, keep reference to UploadState
    [upload_data(#part{start = Pos,
                       len = 0,
                       upload_state = tbd,
                       status = active}, Data) | Done].

upload_data(#part{len = Len, upload_state = _US} = Current, Data) ->
    %% @todo send chunked data to socket (get UploadState from initiate_part)
error_logger:info_msg("Simulating upload delay for ~p bytes of data~n", [byte_size(Data)]), timer:sleep(2000),
    Current#part{len = Len + byte_size(Data)}.

finalize_part(#part{upload_state = _US} = Current, Data) ->
    Finished = #part{len = ?PART_SIZE} = upload_data(Current, Data),
    %% @todo finalize HTTP request, process response
    %% @todo if a part fails, it should be retried, we would need to
    %%       accumulate the whole part in memory for this...
    Finished#part{status = done}.

split_data(Data, Len) ->
    {binary_part(Data, 0, Len),
     binary_part(Data, Len, byte_size(Data) - Len)}.

state_to_info(#state{cmd = Cmd, parts = Parts}) ->
    {Cmd, lists:reverse([{L, S} || #part{len = L, status = S} <- Parts])}.

connect_socket(TcpPort) ->
    connect_socket(TcpPort, 3).
connect_socket(_TcpPort, 0) ->
    {error, econnrefused};
connect_socket(TcpPort, RetriesLeft) ->
    case gen_tcp:connect(localhost, TcpPort,
                    [binary, {packet, 0}, {active, once}], ?CONNECT_TIMEOUT) of
        {error, econnrefused} ->
            timer:sleep(500),
            connect_socket(TcpPort, RetriesLeft -1);
        {error, Reason} ->
            {error, Reason};
        {ok, Socket} ->
            {ok, Socket}
    end.
