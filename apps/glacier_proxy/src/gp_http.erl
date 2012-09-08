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

    {Status, Headers, Body, Req3} =
        case Method of
            'GET'  -> get(Path, Params, Req2);
            'POST' -> post(Path, Params, Req2);
            Method -> not_found(Method, Path, Params, Req2)
        end,

    {ok, Req4} = cowboy_http_req:reply(Status, Headers, Body, Req3),
    {ok, Req4, State}.

terminate(_Req, _State) ->
    ok.


%% ===================================================================
%% Request routing
%% ===================================================================

get([<<"status">>], [], Req) ->

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
     jiffy:encode(DummyReply), Req};

get(Path, Params, Req) -> not_found('GET', Path, Params, Req).


post([<<"vault">>, Vault], _Params, Req) ->
    {ok, {Sha, TreeSha}, Req2} = upload_body(Req),

    DummyReply = {
        [{<<"sha1">>, Sha},
         {<<"treeSha">>, TreeSha},
         {<<"date">>, <<"Sun, 2 Sep 2012 12:00:00 GMT">>},
         {<<"archiveId">>, <<"EXAMPLEArchiveId">>},
         {<<"location">>, <<"/12345678/vaults/", Vault/binary, "/archives/EXAMPLEArchiveId">>}]
    },

    error_logger:info_msg("Will reply ~p~n", [DummyReply]),

    {200,
     [{<<"Content-Type">>, <<"application/json">>}],
     jiffy:encode(DummyReply), Req2};

post(Path, Params, Req) -> not_found('POST', Path, Params, Req).


%% ===================================================================
%% Internal functions
%% ===================================================================

not_found(_Method, _Path, _Params, Req) ->
    {404, [], <<"Not found">>, Req}.


upload_body(Req) ->
    Context = gp_upload:init(Req),
    upload_body(cowboy_http_req:stream_body(Req), Context).

upload_body({ok, Data, Req}, Context) ->
    error_logger:info_msg("g8_http: Got body chunk with ~p bytes~n", [byte_size(Data)]),
    upload_body(cowboy_http_req:stream_body(Req), gp_upload:update(Data, Context));
upload_body({done, Req}, Context) ->
    Context2 = gp_upload:final(Context),
    Sha = gp_upload:sha(Context2),
    TreeSha = gp_upload:tree_sha(Context2),
    {ok, {Sha, TreeSha}, Req}.
