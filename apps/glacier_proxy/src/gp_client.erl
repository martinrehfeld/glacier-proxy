-module(gp_client).

-include("glacier_proxy.hrl").

-define(ACCOUNT, <<"-">>).
-define(VAULT, <<"homenet-files">>).
-define(REGION, <<"eu-west-1">>).
-define(SERVICE, <<"glacier">>).
-define(VERSION, <<"2012-06-01">>).
-define(HOST, <<?SERVICE/binary, $., ?REGION/binary, ".amazonaws.com">>).
-define(PATH, <<$/, ?ACCOUNT/binary, "/vaults/", ?VAULT/binary, "/archives">>).
-define(URL, ?b2l(<<"https://", ?HOST/binary, ?PATH/binary>>)).
-define(TIMEOUT, 5000).
-define(CONTENT_FILE, "rebar").
-define(CONTENT_SHA256, <<"09daa49651fe4f4a4e7a369d55eef3da7c26f679943d3a2863f8e75b4d80dc13">>).
-define(TREE_HASH, ?CONTENT_SHA256).
-define(NEWLINE, <<10>>).
-define(TS_FMT, "~4.10.0b~2.10.0b~2.10.0bT~2.10.0b~2.10.0b~2.10.0bZ").

-define(ACCESS_KEY, gp_config:aws_access_key_id()).
-define(SECRET_ACCESS_KEY, gp_config:aws_secret_access_key()).

%% API
-export([upload_archive/0]).


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
        {<<"x-amz-date">>, timestamp(Date)},
        {<<"x-amz-archive-description">>, <<"Glacier Proxy test.">>},
        {<<"x-amz-sha256-tree-hash">>, ?TREE_HASH},
        {<<"x-amz-content-sha256">>, ?CONTENT_SHA256}
    ],
    SignedHdrs =
        [{<<"Authorization">>,
                sign(<<"POST">>, ?HOST, ?PATH, ?VERSION, Date, ?CONTENT_SHA256)}
         | Hdrs],

    {ok, Body} = file:read_file(?CONTENT_FILE),

    Response = lhttpc:request(?URL, post, SignedHdrs, Body, ?TIMEOUT),
    error_logger:info_msg("Response is: ~p~n", [Response]),
    Response.

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


%% ===================================================================
%% Internal functions
%% ===================================================================

sign(Method, Host, Path, Version, Date, ContentSha) ->
    CR = canonical_request(Method, Host, Path, Version, Date, ContentSha),
    %% error_logger:info_msg("CR is: ~p~n", [CR]),

    STS = string_to_sign(CR, Date),
    %% error_logger:info_msg("STS is: ~p~n", [STS]),

    DerivedKey = derived_key(?SECRET_ACCESS_KEY, Date),
    Signature = signature(DerivedKey, STS),
    authorization(?ACCESS_KEY, Signature, Date).


%% POST
%% /-/vaults/examplevault
%%
%% host:glacier.us-east-1.amazonaws.com
%% x-amz-content-sha256:726e392cb4d09924dbad1cc0ba3b00c3643d03d14cb4b823e2f041cff612a628
%% x-amz-date:20120507T000000Z
%% x-amz-glacier-version:2012-06-01
%%
%% host;x-amz-content-sha256;x-amz-date;x-amz-glacier-version

canonical_request(Method, Host, Path, Version, Date, ContentSha) ->
    Timestamp = timestamp(Date),

    <<Method/binary, ?NEWLINE/binary,
      Path/binary, ?NEWLINE/binary,
      ?NEWLINE/binary,
      "host:", Host/binary, ?NEWLINE/binary,
      "x-amz-content-sha256:", ContentSha/binary, ?NEWLINE/binary,
      "x-amz-date:", Timestamp/binary, ?NEWLINE/binary,
      "x-amz-glacier-version:", Version/binary, ?NEWLINE/binary,
      ?NEWLINE/binary,
      "host;x-amz-content-sha256;x-amz-date;x-amz-glacier-version", ?NEWLINE/binary,
      ContentSha/binary>>.

%% AWS4-HMAC-SHA256
%% 20120525T002453Z
%% 20120525/us-east-1/glacier/aws4_request
%% 5f1da1a2d0feb614dd03d71e87928b8e449ac87614479332aced3a701f916743

string_to_sign(CR, Date) ->
    Hash = gp_chksum:sha256(CR),
    Timestamp = timestamp(Date),
    Datestamp = datestamp(Date),

    <<"AWS4-HMAC-SHA256", ?NEWLINE/binary,
      Timestamp/binary, ?NEWLINE/binary,
      Datestamp/binary, $/, ?REGION/binary, $/, ?SERVICE/binary, "/aws4_request", ?NEWLINE/binary,
      Hash/binary>>.


%% derived key = HMAC(HMAC(HMAC(HMAC("AWS4" + YourSecretAccessKey,"20120525"),"us-east-1"),"glacier"),"aws4_request"))
derived_key(SecretAccessKey, Date) ->
    Datestamp = datestamp(Date),

    HMAC1 = gp_chksum:hmac256_digest(<<"AWS4", SecretAccessKey/binary>>, Datestamp),
    HMAC2 = gp_chksum:hmac256_digest(HMAC1, ?REGION),
    HMAC3 = gp_chksum:hmac256_digest(HMAC2, ?SERVICE),
    gp_chksum:hmac256_digest(HMAC3, <<"aws4_request">>).

signature(DerivedKey, STS) ->
    gp_chksum:hmac256(DerivedKey, STS).


%% Authorization: AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20120525/us-east-1/glacier/aws4_request,
%% SignedHeaders=host;x-amz-date;x-amz-glacier-version,
%% Signature=3ce5b2f2fffac9262b4da9256f8d086b4aaf42eba5f111c21681a65a127b7c2a
authorization(AccessKey, Signature, Date) ->
    Datestamp = datestamp(Date),

    <<"AWS4-HMAC-SHA256 ",
      "Credential=", AccessKey/binary, $/, Datestamp/binary, $/,
                     ?REGION/binary, $/, ?SERVICE/binary, "/aws4_request,"
      "SignedHeaders=host;x-amz-content-sha256;x-amz-date;x-amz-glacier-version,"
      "Signature=", Signature/binary>>.


timestamp({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    list_to_binary(io_lib:format(?TS_FMT, [Year, Month, Day, Hour, Minute, Second])).

datestamp(Date) ->
    binary:part(timestamp(Date), 0, 8).
