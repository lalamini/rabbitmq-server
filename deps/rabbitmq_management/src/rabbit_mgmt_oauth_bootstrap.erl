%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2024 Broadcom. All Rights Reserved. The term “Broadcom” refers to Broadcom Inc. and/or its subsidiaries. All rights reserved.
%%

-module(rabbit_mgmt_oauth_bootstrap).

-export([init/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

%%--------------------------------------------------------------------

init(Req0, State) ->
  bootstrap_oauth(rabbit_mgmt_headers:set_no_cache_headers(
     rabbit_mgmt_headers:set_common_permission_headers(Req0, ?MODULE), ?MODULE), State).

bootstrap_oauth(Req0, State) ->
  AuthSettings = rabbit_mgmt_wm_auth:authSettings(),
  JSContent = case proplists:get_value(oauth_enabled, AuthSettings, false) of 
    true -> oauth_initialize_if_required(AuthSettings) ++ set_token_auth(Req0) ++ oauth_initiate_if_required();
    false -> oauth_initialize_if_required(AuthSettings)
  end,
  {ok, cowboy_req:reply(200, #{<<"content-type">> => <<"text/javascript; charset=utf-8">>}, JSContent, Req0), State}.

oauth_initialize_if_required(AuthSettings) ->
  ["function oauth_initialize_if_required() { return oauth_initialize(" ,
    rabbit_json:encode(rabbit_mgmt_format:format_nulls(AuthSettings)) , ") }" ].

set_token_auth(Req0) ->
  case cowboy_req:parse_header(<<"authorization">>, Req0) of
    {bearer, Token} ->  ["set_token_auth('", Token, "');"];
    _ -> []
  end.

oauth_initiate_if_required() ->
  ["var oauth = oauth_initialize_if_required(); oauth_initiate_if_required();"].

