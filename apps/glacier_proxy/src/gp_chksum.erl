-module(gp_chksum).

%% API
-export([sha_init/0, sha_update/2, sha_final/1]).
-export([sha256_init/0, sha256_update/2, sha256_final/1]).


%% ===================================================================
%% API Function Exports
%% ===================================================================

sha_init() ->
    crypto:sha_init().

sha_update(Context, Data) ->
    crypto:sha_update(Context, Data).

sha_final(Context) ->
    bin_to_hex(crypto:sha_final(Context)).


sha256_init() ->
    erlsha2:sha256_init().

sha256_update(Context, Data) ->
    erlsha2:sha256_update(Context, Data).

sha256_final(Context) ->
    bin_to_hex(erlsha2:sha256_final(Context)).


%% ===================================================================
%% Internal functions
%% ===================================================================

bin_to_hex(BinDigest) ->
    iolist_to_binary([[io_lib:format("~2.16.0b",[X]) || <<X:8>> <= BinDigest ]]).
