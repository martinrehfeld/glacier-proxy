-module(gp_http).

%% Cowboy handler callbacks
-export([init/3, handle/2, terminate/2]).


%% ===================================================================
%% Cowboy handler callbacks
%% ===================================================================

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req}  = cowboy_http_req:method(Req),
    {Path, Req}    = cowboy_http_req:path(Req),
    {Params, Req2} = cowboy_http_req:qs_vals(Req),

    {Status, Headers, Body} =
        case Method of
            'GET'  -> get(Path, Params, Req2);
            'POST' -> post(Path, Params, Req2);
            Method -> not_found(Method, Path, Params, Req2)
        end,

    {ok, Req3} = cowboy_http_req:reply(Status, Headers, Body, Req2),
    {ok, Req3, State}.

terminate(_Req, _State) ->
    ok.


%% ===================================================================
%% Request routing
%% ===================================================================

get([<<"status">>], [], _Req) ->

    %% {"jobs":[{"jobId":"example1",
    %%           "command":"upload",
    %%           "bytesDone:1234",
    %%           "bytesTotal":4321,  // when the request sent Content-Length header
    %%           "status":"active",
    %%           "msg":"extended human readable message for the status"},
    %%          ...]}

    DummyReply = {
        [{jobs, []}]
    },

    {200,
     [{<<"Content-Type">>, <<"application/json">>}],
     jiffy:encode(DummyReply)};

get(Path, Params, Req) -> not_found('GET', Path, Params, Req).


post([<<"vault">>, Vault], _Params, Req) ->
    DummyReply = {
        [{<<"sha1">>, <<"f572d396fae9206628714fb2ce00f72e94f2258f">>},
         {<<"date">>, <<"Sun, 2 Sep 2012 12:00:00 GMT">>},
         {<<"archiveId">>, <<"EXAMPLEArchiveId">>},
         {<<"location">>, <<"/12345678/vaults/", Vault/binary, "/archives/EXAMPLEArchiveId">>}]
    },

    ok = process_body(cowboy_http_req:stream_body(Req)),

    {200,
     [{<<"Content-Type">>, <<"application/json">>}],
     jiffy:encode(DummyReply)};

post(Path, Params, Req) -> not_found('POST', Path, Params, Req).


%% ===================================================================
%% Internal functions
%% ===================================================================

not_found(_Method, _Path, _Params, _Req) ->
    {404, [], <<"Not found">>}.


%% @doc for testing chunked requests only, @todo move into some upload handler
process_body({ok, Data, Req}) ->
    error_logger:info_msg("Got body chunk with ~p bytes~n", [byte_size(Data)]),
    process_body(cowboy_http_req:stream_body(Req));
process_body({done, _Req}) ->
    ok.
