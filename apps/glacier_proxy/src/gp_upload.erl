-module(gp_upload).

-include("glacier_proxy.hrl").

%% AWS Glacier allows a maximum of 10,000 parts per upload, so the
%% PART_SIZE will determine the biggest archive size that can be
%% uploaded by this service, e.g. 128 MB part size will allow an
%% archive size of up to ~1.2 TB.
-define(PART_SIZE, (128 * 1024 * 1024)). % 128 MB

%% AWS Glacier uses chunks of 1 MB to calculate the checksums on.
%% These checksums are reduced to the final tree checksum.
-define(CHUNK_SIZE, (1024 * 1024)). % 1 MB

-define(CONNECT_TIMEOUT, 1000).

-record(context, {req, parts, shactx, sha}).
-record(part, {start, len, shas, upload_state, status}).

%% API
-export([init/1, update/2, final/1]).
-export([sha/1, tree_sha/1]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

init(Req) ->
    #context{req = Req,
             parts = [],
             shactx = gp_chksum:sha_init()}.

update(Data, #context{parts = [], shactx = Sha} = Ctx) ->
    error_logger:info_msg("Received ~p bytes of data~n", [byte_size(Data)]),
    initiate_upload(),
    Ctx#context{parts = initiate_part(Data, []),
                shactx = gp_chksum:sha_update(Sha, Data)};
update(Data, #context{parts = Parts, shactx = Sha} = Ctx) ->
    error_logger:info_msg("Received ~p bytes of data~n", [byte_size(Data)]),
    Ctx#context{parts = do_upload(Data, Parts),
                shactx = gp_chksum:sha_update(Sha, Data)}.


final(#context{shactx = Sha} = Ctx) ->
    error_logger:info_msg("gs_uploader: completed successfully~n", []),
    finalize_upload(),
    Ctx#context{sha = gp_chksum:sha_final(Sha)}.

abort(#context{}) ->
    error_logger:info_msg("gs_uploader: aborted~n", []),
    abort_upload().

sha(#context{sha = Sha}) -> Sha.

%% @todo acutally compute the treesha
tree_sha(#context{} = Context) -> sha(Context).


%% ===================================================================
%% Internal functions
%% ===================================================================
initiate_upload() ->
    %% @todo issue AWS Glacier Initiate Multipart Upload
    ok.

do_upload(Data, Parts) ->
    [Current | Done] = Parts,
    DataSize = byte_size(Data),
    LenToGo = ?PART_SIZE - Current#part.len,
    if
        DataSize >= LenToGo ->
            {RemainingData, NewData} = split_data(Data, LenToGo),
            initiate_part(NewData, [finalize_part(Current, RemainingData) | Done]);

        DataSize < LenToGo ->
            [upload_data(Current, Data) | Done]
    end.

finalize_upload() ->
    %% @todo issue AWS Glacier Complete Multipart Upload
    ok.

abort_upload() ->
    %% @todo issue AWS Glacier Abort Multipart Upload if started
    ok.

initiate_part(Data, Done) when byte_size(Data) > ?PART_SIZE ->
    {Data1, Data2} = split_data(Data, ?PART_SIZE),
    initiate_part(Data2, initiate_part(Data1, Done));
initiate_part(Data, Done) when byte_size(Data) =:= ?PART_SIZE ->
    [Current | Done] = initiate_part(<<>>, Done),
    [finalize_part(Current, Data) | Done ];
initiate_part(Data, Done) ->
    Pos = ?PART_SIZE * length(Done),
    %% @todo issue AWS Glacier Upload Part, keep reference to UploadState
    [upload_data(#part{start = Pos,
                       len = 0,
                       upload_state = tbd,
                       status = active}, Data) | Done].

upload_data(#part{len = Len, upload_state = _US} = Current, Data) ->
    %% @todo send chunked data to socket (get UploadState from initiate_part)
    error_logger:info_msg("Would upload ~p bytes of data (delaying...)~n", [byte_size(Data)]), timer:sleep(200),
    Current#part{len = Len + byte_size(Data)}.

finalize_part(#part{upload_state = _US} = Current, Data) ->
    Finished = #part{len = ?PART_SIZE} = upload_data(Current, Data),
    %% @todo finalize HTTP request, process response
    %% @todo if a part fails, it should be retried, we would need to
    %%       accumulate the whole part in memory for this...
    Finished#part{status = done}.

split_data(Data, Len) ->
    {binary_part(Data, 0, Len),
     binary_part(Data, Len, byte_size(Data) - Len)}.
