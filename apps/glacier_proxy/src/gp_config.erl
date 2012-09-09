-module(gp_config).

-include("glacier_proxy.hrl").

%% API
-export([port/0,
         aws_account_number/0,
         aws_access_key_id/0,
         aws_secret_access_key/0]).


%% ===================================================================
%% API Function Definitions
%% ===================================================================

port() -> 8001.

aws_account_number() -> ?l2b(os:getenv("AWS_ACCOUNT_NUMBER")).
aws_access_key_id() -> ?l2b(os:getenv("AWS_ACCESS_KEY_ID")).
aws_secret_access_key() -> ?l2b(os:getenv("AWS_SECRET_ACCESS_KEY")).
