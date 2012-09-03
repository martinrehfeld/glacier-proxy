-module(gp_http).

-behaviour(elli_handler).

%% Elli callbacks
-export([handle/2, handle_event/3]).


%% ===================================================================
%% Elli callbacks
%% ===================================================================

handle(Req, _Args) ->
    Path   = elli_request:path(Req),
    Method = elli_request:method(Req),
    case Method of
        'GET'  -> get(Path, Req);
        'POST' -> post(Path, Req);
        Method -> not_found(Method, Path, Req)
    end.

handle_event(_, _, _) ->
    ok.


%% ===================================================================
%% Request routing
%% ===================================================================

get([<<"status">>], _Req) ->

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

get(Path, Req) -> not_found('GET', Path, Req).


post([<<"vault">>, Vault], _Req) ->
    DummyReply = {
        [{<<"sha1">>, <<"f572d396fae9206628714fb2ce00f72e94f2258f">>},
         {<<"date">>, <<"Sun, 2 Sep 2012 12:00:00 GMT">>},
         {<<"archiveId">>, <<"EXAMPLEArchiveId">>},
         {<<"location">>, <<"/12345678/vaults/", Vault/binary, "/archives/EXAMPLEArchiveId">>}]
    },

    {200,
     [{<<"Content-Type">>, <<"application/json">>}],
     jiffy:encode(DummyReply)};

post(Path, Req) -> not_found('POST', Path, Req).


%% ===================================================================
%% Internal functions
%% ===================================================================

not_found(_Method, _Path, _Req) ->
    {404, [], <<"Not found">>}.
