%%% @doc Helpers for AWS Signature Version 4 Signing Process
%%% <http://docs.amazonwebservices.com/general/latest/gr/signature-version-4.html>
-module(gp_aws4_signature).

-include("glacier_proxy.hrl").

-define(NEWLINE, <<10>>).

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
    sign(Host, Date, CR, SignedHeaders).

sign(Method, Host, Path, Version, Date, ContentSha) ->
    SignedHeaders = <<"host;x-amz-content-sha256;x-amz-date;x-amz-glacier-version">>,
    CR = canonical_request(Method, Host, Path, Version, Date, SignedHeaders, ContentSha),
    sign(Host, Date, CR, SignedHeaders).


%% ===================================================================
%% Internal functions
%% ===================================================================

sign(Host, Date, CanonicalRequest, SignedHeaders) ->
    [Service|_] = binary:split(Host, <<$.>>),
    STS = string_to_sign(Service, CanonicalRequest, Date),
    DerivedKey = derived_key(Service, ?SECRET_ACCESS_KEY, Date),
    Signature = signature(DerivedKey, STS),
    authorization(?ACCESS_KEY, SignedHeaders, Signature, Service, Date).


canonical_request(Method, Host, Path, Version, Date, SignedHeaders) ->
    EmptyContentSha = gp_chksum:sha256(<<>>),
    canonical_request(Method, Host, Path, Version, Date, SignedHeaders, EmptyContentSha, false).

canonical_request(Method, Host, Path, Version, Date, SignedHeaders, ContentSha) ->
    canonical_request(Method, Host, Path, Version, Date, SignedHeaders, ContentSha, true).

canonical_request(Method, Host, Path, Version, Date, SignedHeaders, ContentSha, IncludeContentSha) ->
    Timestamp = gp_util:timestamp(Date),

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


string_to_sign(Service, CR, Date) ->
    Hash = gp_chksum:sha256(CR),
    Timestamp = gp_util:timestamp(Date),
    Datestamp = gp_util:datestamp(Date),

    <<"AWS4-HMAC-SHA256", ?NEWLINE/binary,
      Timestamp/binary, ?NEWLINE/binary,
      Datestamp/binary, $/, ?REGION/binary, $/, Service/binary, "/aws4_request", ?NEWLINE/binary,
      Hash/binary>>.


derived_key(Service, SecretAccessKey, Date) ->
    Datestamp = gp_util:datestamp(Date),

    HMAC1 = gp_chksum:hmac256_digest(<<"AWS4", SecretAccessKey/binary>>, Datestamp),
    HMAC2 = gp_chksum:hmac256_digest(HMAC1, ?REGION),
    HMAC3 = gp_chksum:hmac256_digest(HMAC2, Service),
    gp_chksum:hmac256_digest(HMAC3, <<"aws4_request">>).


signature(DerivedKey, STS) ->
    gp_chksum:hmac256(DerivedKey, STS).


authorization(AccessKey, SignedHeaders, Signature, Service, Date) ->
    Datestamp = gp_util:datestamp(Date),

    <<"AWS4-HMAC-SHA256 ",
      "Credential=", AccessKey/binary, $/, Datestamp/binary, $/,
                     ?REGION/binary, $/, Service/binary, "/aws4_request, "
      "SignedHeaders=", SignedHeaders/binary, ", ",
      "Signature=", Signature/binary>>.
