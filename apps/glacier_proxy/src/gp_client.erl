-module(gp_client).

-include("glacier_proxy.hrl").

-define(VAULT, <<"homenet-files">>).
-define(PATH, <<$/, ?ACCOUNT/binary, "/vaults/", ?VAULT/binary, "/archives">>).
-define(PATH_MULTI_INIT, <<$/, ?ACCOUNT/binary, "/vaults/", ?VAULT/binary, "/multipart-uploads">>).
-define(URL, ?b2l(<<"https://", ?ENDPOINT/binary, ?PATH/binary>>)).
-define(URL_MULTI_INIT, ?b2l(<<"https://", ?ENDPOINT/binary, ?PATH_MULTI_INIT/binary>>)).
-define(TIMEOUT, 5000).
-define(CONTENT_FILE, "rebar").
-define(CONTENT_RANGE, <<"bytes 0-146278/*">>).
-define(CONTENT_LENGTH, <<"146279">>).
-define(CONTENT_SHA256, <<"e049326f0f95e1b91ec35268625f1b8b162b17e34b8224a0c25192330067c6a7">>).
-define(TREE_HASH, ?CONTENT_SHA256).


%% API
-export([upload_archive/0]).
-export([upload_archive_init/0, upload_archive_update/1, upload_archive_final/1]).
-export([do_multi_upload/0]).


%% ===================================================================
%% API Function Definitions
%% ===================================================================

%% POST /AccountId/vaults/VaultName/archives
%% Host: glacier.Region.amazonaws.com
%% x-amz-glacier-version: 2012-06-01
%% Date: Date
%% Authorization: SignatureValue
%% x-amz-archive-description: Description
%% x-amz-sha256-tree-hash: SHA256 tree hash
%% x-amz-content-sha256: SHA256 linear hash
%% Content-Length: Length
%%
%% <Request body.>

upload_archive() ->
    Date = erlang:universaltime(),
    Hdrs = [
        {<<"x-amz-glacier-version">>, ?VERSION},
        {<<"x-amz-date">>, gp_util:timestamp(Date)},
        {<<"x-amz-archive-description">>, <<"Glacier Proxy test.">>},
        {<<"x-amz-sha256-tree-hash">>, ?TREE_HASH},
        {<<"x-amz-content-sha256">>, ?CONTENT_SHA256},
        {<<"Content-Type">>, <<"application/octet-stream">>},
        {<<"Authorization">>, gp_aws4_signature:sign(<<"POST">>, ?ENDPOINT, ?PATH, ?VERSION, Date, ?CONTENT_SHA256)}
    ],

    {ok, Body} = file:read_file(?CONTENT_FILE),

    Response = lhttpc:request(?URL, post, Hdrs, Body, ?TIMEOUT),
    error_logger:info_msg("Upload Archive response is: ~p~n", [Response]),
    {ok, {{201, _}, _ResponseHdrs, _}} = Response.

%% Successful response: {ok, {Code, Hdrs, Body}}
%% {ok,{{201,"Created"},
%%     [{"Date","Sun, 09 Sep 2012 00:24:48 GMT"},
%%      {"Content-Length" ,"2"},
%%      {"Content-Type","application/json"},
%%      {"X-Amz-Archive-Id","PDZAD_U3LEHyS5qgK0hW-5Kd9oFC8WbOzq9X2Og5XVS5Nb2QeEOwlFS60ve01lqiBC8WGkDpK1wPqzpmR3HZVPi-w_OWd1M-w1bH-ZBEHSN5xryqKJXW4tLmciebg8Tc9y0iyzwxgQ"},
%%      {"Location","/618081757537/vaults/homenet-files/archives/PDZAD_U3LEHyS5qgK0hW-5Kd9oFC8WbOzq9X2Og5XVS5Nb2QeEOwlFS60ve01lqiBC8WGkDpK1wPqzpmR3HZVPi-w_OWd1M-w1bH-ZBEHSN5xryqKJXW4tLmciebg8Tc9y0iyzwxgQ"},
%%      {"x-amz-sha256-tree-hash","09daa49651fe4f4a4e7a369d55eef3da7c26f679943d3a2863f8e75b4d80dc13"},
%%      {"X-Amzn-Requestid","OWxnmUiqxk6AiQ0cYgTWRF1eEGG6kNP_nsDo2GxaL2qtXUo"}],
%%     <<"{}">>}}


upload_archive_init() ->
    Date = erlang:universaltime(),
    Hdrs = [
        {<<"x-amz-glacier-version">>, ?VERSION},
        {<<"x-amz-date">>, gp_util:timestamp(Date)},
        {<<"x-amz-archive-description">>, <<"Glacier Proxy test.">>},
        {<<"x-amz-part-size">>, <<"1048576">>},
        {<<"Authorization">>, gp_aws4_signature:sign(<<"POST">>, ?ENDPOINT, ?PATH_MULTI_INIT, ?VERSION, Date)}
    ],

    Response = lhttpc:request(?URL_MULTI_INIT, post, Hdrs, [], ?TIMEOUT),
    error_logger:info_msg("Initiate Multi Upload Response is: ~p~n", [Response]),
    {ok, {{201, _}, ResponseHdrs, _}} = Response,
    {ok, ResponseHdrs}.

%% {ok,{{201,"Created"},
%%      [{"Date","Sun, 09 Sep 2012 14:41:50 GMT"},
%%       {"Content-Length","2"},
%%       {"Content-Type","application/json"},
%%       {"Location", "/618081757537/vaults/homenet-files/multipart-uploads/S1ekbiEErLIPQCQHZyNGdpMw7zwY0WQfp8-A6Xah6Xf7tGmSvKCm8lna2llg9SjCDAEolze9FqEpR1vKKNUT1gm6doNi"},
%%       {"x-amz-multipart-upload-id", "S1ekbiEErLIPQCQHZyNGdpMw7zwY0WQfp8-A6Xah6Xf7tGmSvKCm8lna2llg9SjCDAEolze9FqEpR1vKKNUT1gm6doNi"},
%%       {"X-Amzn-Requestid", "lTGV_hPzjXe-YcIbyveE2vnLZOQZ58mfRUtDmyWrgH3i0VE"}],
%%      <<"{}">>}}

upload_archive_update(Context) ->
    Date = erlang:universaltime(),

    Path = ?l2b(proplists:get_value("Location", Context)),
    Url = ?b2l(<<"https://", ?ENDPOINT/binary, Path/binary>>),

    Hdrs = [
        {<<"x-amz-glacier-version">>, ?VERSION},
        {<<"x-amz-date">>, gp_util:timestamp(Date)},
        {<<"x-amz-sha256-tree-hash">>, ?TREE_HASH},
        {<<"x-amz-content-sha256">>, ?CONTENT_SHA256},
        {<<"Content-Range">>, ?CONTENT_RANGE},
        {<<"Content-Type">>, <<"application/octet-stream">>},
        {<<"Authorization">>, gp_aws4_signature:sign(<<"PUT">>, ?ENDPOINT, Path, ?VERSION, Date, ?CONTENT_SHA256)}
    ],

    {ok, Body} = file:read_file(?CONTENT_FILE),

    Response = lhttpc:request(Url, put, Hdrs, Body, ?TIMEOUT),
    error_logger:info_msg("Upload Part Response is: ~p~n", [Response]),
    {ok, {{204, _}, _ResponseHdrs, _}} = Response,
    Context.

%% {ok,{{204,"No Content"},
%%      [{"Date","Sun, 09 Sep 2012 15:14:54 GMT"},
%%       {"x-amz-sha256-tree-hash","09daa49651fe4f4a4e7a369d55eef3da7c26f679943d3a2863f8e75b4d80dc13"},
%%       {"X-Amzn-Requestid","J3lHX-bnL6xSVpuLl3J6TtZtYQvFT-dwr0suig2jnbUrvLI"}],
%%      <<>>}}

upload_archive_final(Context) ->
    Date = erlang:universaltime(),

    Path = ?l2b(proplists:get_value("Location", Context)),
    Url = ?b2l(<<"https://", ?ENDPOINT/binary, Path/binary>>),

    Hdrs = [
        {<<"x-amz-glacier-version">>, ?VERSION},
        {<<"x-amz-date">>, gp_util:timestamp(Date)},
        {<<"x-amz-sha256-tree-hash">>, ?TREE_HASH},
        {<<"x-amz-archive-size">>, ?CONTENT_LENGTH},
        {<<"Authorization">>, gp_aws4_signature:sign(<<"POST">>, ?ENDPOINT, Path, ?VERSION, Date)}
    ],

    Response = lhttpc:request(Url, post, Hdrs, [], ?TIMEOUT),
    error_logger:info_msg("Complete Multi Upload Response is: ~p~n", [Response]),
    {ok, {{201, _}, _ResponseHdrs, _}} = Response,
    Response.


do_multi_upload() ->
    {ok, Context} = upload_archive_init(),
    NewContext = upload_archive_update(Context),
    upload_archive_final(NewContext).

%% ===================================================================
%% Internal functions
%% ===================================================================
