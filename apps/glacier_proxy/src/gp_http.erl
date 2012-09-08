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
    {ok, {Sha, Sha256}, Req2} = process_body(Req),

    DummyReply = {
        [{<<"sha1">>, Sha},
         {<<"sha256">>, Sha256},
         {<<"date">>, <<"Sun, 2 Sep 2012 12:00:00 GMT">>},
         {<<"archiveId">>, <<"EXAMPLEArchiveId">>},
         {<<"location">>, <<"/12345678/vaults/", Vault/binary, "/archives/EXAMPLEArchiveId">>}]
    },

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
process_body(Req) ->
    Sha = gp_chksum:sha_init(),
    Sha256 = gp_chksum:sha256_init(),
    process_body(cowboy_http_req:stream_body(Req), {Sha, Sha256}).
process_body({ok, Data, Req}, {Sha, Sha256}) ->
    error_logger:info_msg("Got body chunk with ~p bytes~n", [byte_size(Data)]),
    process_body(cowboy_http_req:stream_body(Req),
        {gp_chksum:sha_update(Sha, Data), gp_chksum:sha256_update(Sha256, Data)});
process_body({done, Req}, {Sha, Sha256}) ->
    {ok, {gp_chksum:sha_final(Sha), gp_chksum:sha256_final(Sha256)}, Req}.
