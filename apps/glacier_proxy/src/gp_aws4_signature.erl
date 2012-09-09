-module(gp_aws4_signature).

-include("glacier_proxy.hrl").

-define(REGION, <<"eu-west-1">>).
-define(SERVICE, <<"glacier">>).
-define(VERSION, <<"2012-06-01">>).
-define(NEWLINE, <<10>>).
-define(TS_FMT, "~4.10.0b~2.10.0b~2.10.0bT~2.10.0b~2.10.0b~2.10.0bZ").

-define(ACCOUNT, (gp_config:aws_account_number())).
-define(ACCESS_KEY, (gp_config:aws_access_key_id())).
-define(SECRET_ACCESS_KEY, (gp_config:aws_secret_access_key())).

%% API
-export([sign/5, sign/6]).


%% ===================================================================
%% API Function Definitions
%% ===================================================================

sign(Method, Host, Path, Version, Date) ->
    SignedHeaders = <<"host;x-amz-date;x-amz-glacier-version">>,
    CR = canonical_request(Method, Host, Path, Version, Date, SignedHeaders),
    %% error_logger:info_msg("CR is: ~p~n", [CR]),
    sign(Date, CR, SignedHeaders).

sign(Method, Host, Path, Version, Date, ContentSha) ->
    SignedHeaders = <<"host;x-amz-content-sha256;x-amz-date;x-amz-glacier-version">>,
    CR = canonical_request(Method, Host, Path, Version, Date, SignedHeaders, ContentSha),
    %% error_logger:info_msg("CR is: ~p~n", [CR]),
    sign(Date, CR, SignedHeaders).


%% ===================================================================
%% Internal functions
%% ===================================================================

sign(Date, CanonicalRequest, SignedHeaders) ->
    STS = string_to_sign(CanonicalRequest, Date),
    DerivedKey = derived_key(?SECRET_ACCESS_KEY, Date),
    Signature = signature(DerivedKey, STS),
    authorization(?ACCESS_KEY, SignedHeaders, Signature, Date).

%% POST
%% /-/vaults/examplevault
%%
%% host:glacier.us-east-1.amazonaws.com
%% x-amz-content-sha256:726e392cb4d09924dbad1cc0ba3b00c3643d03d14cb4b823e2f041cff612a628
%% x-amz-date:20120507T000000Z
%% x-amz-glacier-version:2012-06-01
%%
%% host;x-amz-content-sha256;x-amz-date;x-amz-glacier-version

canonical_request(Method, Host, Path, Version, Date, SignedHeaders) ->
    EmptyContentSha = gp_chksum:sha256(<<>>),
    canonical_request(Method, Host, Path, Version, Date, SignedHeaders, EmptyContentSha, false).

canonical_request(Method, Host, Path, Version, Date, SignedHeaders, ContentSha) ->
    canonical_request(Method, Host, Path, Version, Date, SignedHeaders, ContentSha, true).

canonical_request(Method, Host, Path, Version, Date, SignedHeaders, ContentSha, IncludeContentSha) ->
    Timestamp = timestamp(Date),

    Part1 = <<Method/binary, ?NEWLINE/binary,
              Path/binary, ?NEWLINE/binary,
              ?NEWLINE/binary,
              "host:", Host/binary, ?NEWLINE/binary>>,

    Part2 = case IncludeContentSha of
                true ->
                  <<"x-amz-content-sha256:", ContentSha/binary, ?NEWLINE/binary>>;
                false ->
                  <<>>
            end,

    Part3 = <<"x-amz-date:", Timestamp/binary, ?NEWLINE/binary,
              "x-amz-glacier-version:", Version/binary, ?NEWLINE/binary,
              ?NEWLINE/binary,
              SignedHeaders/binary, ?NEWLINE/binary,
              ContentSha/binary>>,

    <<Part1/binary, Part2/binary, Part3/binary>>.

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
authorization(AccessKey, SignedHeaders, Signature, Date) ->
    Datestamp = datestamp(Date),

    <<"AWS4-HMAC-SHA256 ",
      "Credential=", AccessKey/binary, $/, Datestamp/binary, $/,
                     ?REGION/binary, $/, ?SERVICE/binary, "/aws4_request,"
      "SignedHeaders=", SignedHeaders/binary, ",",
      "Signature=", Signature/binary>>.


timestamp({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    list_to_binary(io_lib:format(?TS_FMT, [Year, Month, Day, Hour, Minute, Second])).

datestamp(Date) ->
    binary:part(timestamp(Date), 0, 8).

