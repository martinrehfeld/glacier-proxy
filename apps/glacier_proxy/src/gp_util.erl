-module(gp_util).

-include("glacier_proxy.hrl").

-define(TS_FMT, "~4.10.0b~2.10.0b~2.10.0bT~2.10.0b~2.10.0b~2.10.0bZ").

%% API
-export([timestamp/1, datestamp/1]).


%% ===================================================================
%% API Function Definitions
%% ===================================================================

timestamp({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    list_to_binary(io_lib:format(?TS_FMT,
            [Year, Month, Day, Hour, Minute, Second])).

datestamp(Date) ->
    binary:part(timestamp(Date), 0, 8).


%% ===================================================================
%% Internal functions
%% ===================================================================
