-module(gp_archive).

-include("glacier_proxy.hrl").

%% AWS Glacier allows a maximum of 10,000 parts per upload, so the
%% part size will determine the biggest archive size that can be
%% uploaded by this service, e.g. 128 MB part size will allow an
%% archive size of up to ~1.2 TB.
-record(archive, {description,
                  location,
                  chunks_per_part=2, % needs to be a power of 2
                  parts=[],
                  status}).

%% API
-export([description/1, location/1]).


%% ===================================================================
%% API Function Definitions
%% ===================================================================

description(#archive{description = Value}) ->
    Value.

location(#archive{location = Value}) ->
    Value.


%% ===================================================================
%% Internal functions
%% ===================================================================
