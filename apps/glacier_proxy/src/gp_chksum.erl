-module(gp_chksum).

-include("glacier_proxy.hrl").

%% API
-export([sha_init/0, sha_update/2, sha_final/1]).
-export([sha256/1, sha256_digest/1, sha256_init/0, sha256_update/2, sha256_final/1]).
-export([hmac256/2, hmac256_digest/2]).
-export([tree_hash/1]).
-export([hexdigest/1]).


%% ===================================================================
%% API Function Definitions
%% ===================================================================

sha_init() ->
    crypto:sha_init().

sha_update(Context, Data) ->
    crypto:sha_update(Context, Data).

sha_final(Context) ->
    hexdigest(crypto:sha_final(Context)).


sha256(Data) ->
    hexdigest(sha256_digest(Data)).

sha256_digest(Data) ->
    erlsha2:sha256(Data).

sha256_init() ->
    erlsha2:sha256_init().

sha256_update(Context, Data) ->
    erlsha2:sha256_update(Context, Data).

sha256_final(Context) ->
    hexdigest(erlsha2:sha256_final(Context)).


hmac256(Key, Data) ->
    hexdigest(hmac256_digest(Key, Data)).

hmac256_digest(Key, Data) ->
    hmac:hmac256(Key, Data).


tree_hash(L) ->
    hexdigest(tree_hash_digest(L)).

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


hexdigest(BinDigest) ->
    ?io2b([[io_lib:format("~2.16.0b",[X]) || <<X:8>> <= BinDigest]]).


%% ===================================================================
%% Internal functions
%% ===================================================================
