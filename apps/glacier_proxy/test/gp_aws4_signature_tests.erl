-module(gp_aws4_signature_tests).
-include_lib("eunit/include/eunit.hrl").
-include("glacier_proxy.hrl").

aws4_signature_test_() ->
    {foreach,
     fun setup/0, fun teardown/1,
     [?_test(sign5()),
      ?_test(sign6())
     ]}.

setup() ->
    meck:new(gp_config),

    meck:expect(gp_config, aws_region, fun () ->
                <<"us-east-1">>
        end),
    meck:expect(gp_config, aws_access_key_id, fun () ->
                <<"AKIDEXAMPLE">>
        end),
    meck:expect(gp_config, aws_secret_access_key, fun () ->
                <<"wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY">>
        end).

teardown(_) ->
    meck:unload(gp_config).


sign5() ->
    Method = <<"POST">>,
    Host = <<"glacier.us-east-1.amazonaws.com">>,
    Path = <<"/-/vaults/examplevault/multipart-uploads">>,
    Version = <<"20110909">>,
    Date = {{2011, 9, 9}, {23, 36, 0}},

    Expected = <<"AWS4-HMAC-SHA256 Credential=AKIDEXAMPLE/20110909/us-east-1/glacier/aws4_request, SignedHeaders=host;x-amz-date;x-amz-glacier-version, Signature=dfcf82ff929116c557e3a8b4cb26e20a06ca08b7b388ec4b0861ac09e71a2383">>,
    Computed = gp_aws4_signature:sign(Method, Host, Path, Version, Date),
    %% error_logger:info_msg("Expected: ~p~nComputed: ~p~n", [Expected, Computed]),
    ?assertEqual(Expected, Computed).


sign6() ->
    Method = <<"POST">>,
    Host = <<"glacier.us-east-1.amazonaws.com">>,
    Path = <<"/-/vaults/examplevault/archives">>,
    Version = <<"20110909">>,
    Date = {{2011, 9, 9}, {23, 36, 0}},
    ContentSha = <<"09daa49651fe4f4a4e7a369d55eef3da7c26f679943d3a2863f8e75b4d80dc13">>,

    Expected = <<"AWS4-HMAC-SHA256 Credential=AKIDEXAMPLE/20110909/us-east-1/glacier/aws4_request, SignedHeaders=host;x-amz-content-sha256;x-amz-date;x-amz-glacier-version, Signature=bd830dcdda7291e1f5bbd7ee906a9c9b7607a9ef796c116c9ca76bcd7d8e4fca">>,
    Computed = gp_aws4_signature:sign(Method, Host, Path, Version, Date, ContentSha),
    %% error_logger:info_msg("Expected: ~p~nComputed: ~p~n", [Expected, Computed]),
    ?assertEqual(Expected, Computed).
