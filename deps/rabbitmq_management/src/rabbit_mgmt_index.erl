%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2024 Broadcom. All Rights Reserved. The term “Broadcom” refers to Broadcom Inc. and/or its subsidiaries. All rights reserved.
%%

-module(rabbit_mgmt_index).

-export([init/2]).

%%--------------------------------------------------------------------

init(Req0, State) ->
    index(rabbit_mgmt_headers:set_no_cache_headers(
        rabbit_mgmt_headers:set_common_permission_headers(Req0, ?MODULE), ?MODULE), State).

index(Req0, State) ->
    Content = html(head(), body()), 
    {ok, cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=utf-8">>}, Content, Req0), State}.

html(Head, Body) ->
  concat(["<!DOCTYPE html>", "<html>", "<head>", Head, "</head>", "<body>", Body, "</body></html>"]).
head() ->
  meta([{"X-UA-Compatible", "IE=edge"}, {"Content-Type", "text/html; charset=utf-8"}]) ++
  title("RabbitMQ Management") ++
  javascript_libs(javascript_libraries()) ++
  oauth_libraries() ++
  head_link([{"css/main.css", "stylesheet", "text/css"}, {"favicon.ico", "shortcut icon", "image/x-icon"}]) ++
  ie_only([javascript_lib("js/excanvas.min.js")] ++ [head_link("css/evil.css", "stylesheet", "text/css")]).
body() ->
    ["<div id='outer'></div><div id='debug'></div><div id='scratch'></div>"].
ie_only(Body) ->
    ["<!--[if lte IE 8]>"] ++ Body ++ ["<![endif]-->"].
meta(List) when is_list(List) ->
    [meta(HttpEquip, Content) || {HttpEquip, Content} <- List].
meta(HttpEquip, Content) ->
    io_lib:format("<meta http-equiv='~s' content='~s' />", [HttpEquip, Content]).
title(Title) ->
    io_lib:format("<title>~s</title>", [Title]).
javascript_libs(List) ->
    [javascript_lib(Src) || Src <- List].
javascript_lib(Src) ->
    io_lib:format("<script src='~s' type='text/javascript'></script>", [Src]).
javascript_body(Body) ->
    io_lib:format("<script type='text/javascript'>~s</script>", [Body]).
head_link(Href, Rel, Type) ->
    io_lib:format("<link href='~s' rel='~s' type='~s'/>", [Href, Rel, Type]).
head_link(List) when is_list(List) ->
    [head_link(Href, Rel, Type) || {Href, Rel, Type} <- List].

concat(Lst) ->
    concat(lists:reverse(Lst), []).
concat([], Acc) -> Acc;
concat([H|T], Acc) ->
    concat(T, H ++ Acc).

oauth_libraries() ->
    case proplists:get_value(oauth_enabled, rabbit_mgmt_wm_auth:authSettings(), false) of 
        false -> javascript_body("var oauth={enabled: false}");
        true -> javascript_libs(["js/oidc-oauth/helper.js", "js/oidc-oauth/oidc-client-ts.js", "js/oidc-oauth/bootstrap.js"])
    end.

javascript_libraries() ->
    ["js/ejs-1.0.min.js", 
        "js/jquery-3.5.1.min.js",
        "js/jquery.flot-0.8.1.min.js" ,
        "js/jquery.flot-0.8.1.time.min.js",
        "js/sammy-0.7.6.min.js",
        "js/json2-2016.10.28.js",
        "js/base64.js",
        "js/global.js",
        "js/main.js",
        "js/prefs.js",
        "js/formatters.js",
        "js/charts.js"].
