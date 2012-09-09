-module(gp_part).

-include("glacier_proxy.hrl").

%% One part of a multi-part upload. The part is the smallest unit
%% of upload to Glacier.
-record(part, {range_start,
               range_end,
               chunks=[],
               content_hash,
               status}).

%% A chunk is of fixed 1 MB size (apart from the last chunk which
%% can be smaller).
-record(chunk, {hash, data}).

%% API
-export([content_range/1, content_hash/1, tree_hash/1]).


%% ===================================================================
%% API Function Definitions
%% ===================================================================

content_range(#part{range_start = Start, range_end = End}) ->
    ?io2b(io_lib:format("bytes ~b-~b/*", [Start, End])).

content_hash(#part{content_hash = Hash}) ->
    gp_chksum:hexdigest(Hash).

tree_hash(#part{chunks = Chunks}) ->
    ChunkHashes = [ Hash || #chunk{hash = Hash} <- Chunks ],
    gp_chksum:tree_hash(ChunkHashes).


%% ===================================================================
%% Internal functions
%% ===================================================================
