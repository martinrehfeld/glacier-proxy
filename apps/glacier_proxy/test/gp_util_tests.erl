-module(gp_util_tests).
-include_lib("eunit/include/eunit.hrl").
-include("glacier_proxy.hrl").

timestamp_test() ->
    Input = {{2012, 12, 1}, {14, 3, 1}},
    Expected = <<"20121201T140301Z">>,
    ?assertEqual(Expected, gp_util:timestamp(Input)).

datestamp_test() ->
    Input = {{2012, 12, 1}, {14, 3, 1}},
    Expected = <<"20121201">>,
    ?assertEqual(Expected, gp_util:datestamp(Input)).
