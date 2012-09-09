-module(gp_chksum).

%% API
-export([sha_init/0, sha_update/2, sha_final/1]).
-export([sha256/1, sha256_digest/1, sha256_init/0, sha256_update/2, sha256_final/1]).
-export([hmac256/2, hmac256_digest/2]).
-export([tree_hash/1]).


%% ===================================================================
%% API Function Definitions
%% ===================================================================

sha_init() ->
    crypto:sha_init().

sha_update(Context, Data) ->
    crypto:sha_update(Context, Data).

sha_final(Context) ->
    bin_to_hex(crypto:sha_final(Context)).


sha256(Data) ->
    bin_to_hex(sha256_digest(Data)).

sha256_digest(Data) ->
    erlsha2:sha256(Data).

sha256_init() ->
    erlsha2:sha256_init().

sha256_update(Context, Data) ->
    erlsha2:sha256_update(Context, Data).

sha256_final(Context) ->
    bin_to_hex(erlsha2:sha256_final(Context)).


hmac256(Key, Data) ->
    bin_to_hex(hmac256_digest(Key, Data)).

hmac256_digest(Key, Data) ->
    hmac:hmac256(Key, Data).


tree_hash(L) ->
    bin_to_hex(tree_hash_digest(L)).

tree_hash_digest([Root]) ->
    Root;
tree_hash_digest([_H | _T] = L) ->
    tree_hash_digest(L, []).

tree_hash_digest([H1, H2 | T], NextLevel) ->
    tree_hash_digest(T, [sha256_digest(<<H1/binary, H2/binary>>) | NextLevel]);
tree_hash_digest([H], NextLevel) ->
    tree_hash_digest(lists:reverse([H | NextLevel]));
tree_hash_digest([], NextLevel) ->
    tree_hash_digest(lists:reverse(NextLevel)).


%% ===================================================================
%% Internal functions
%% ===================================================================


bin_to_hex(BinDigest) ->
    iolist_to_binary([[io_lib:format("~2.16.0b",[X]) || <<X:8>> <= BinDigest ]]).
