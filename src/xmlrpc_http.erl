%% Copyright (C) 2011 Ali Yakout 
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2.1 of the License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.
%%
%% @doc XMLRPC decoder and validator
%%
%% @copyright 2011
%% @author Ali Yakout <ali.yakout@gmail.com>
%% @end
-module(xmlrpc_http).


%%------------------------------------------------------------------------------
%% INCLUDE
%%------------------------------------------------------------------------------
-include("gen_xmlrpc.hrl").


%%------------------------------------------------------------------------------
%% EXPORTS
%%------------------------------------------------------------------------------
-export([call/5]).


%%------------------------------------------------------------------------------
%% EXPORT FUNCTIONS
%%------------------------------------------------------------------------------
%% @spec call(IP, Port, URI, ReqBody, Options) -> Result
%%    IP = ip()
%%    Port = integer()
%%    URI = string()
%%    ReqBody = string()
%%    Options = [{auth, {userString(),passString()}} | HTTPOptions]
%%    HTTPOptions = {timeout, integer()} |
%%                  {ssl, ssl_options()} |
%%                  {autoredirect, boolean()} |
%%                  {proxy_auth, {userstring(), passwordstring()}} |
%%                  {version, http_version()} |
%%                  {relaxed, boolean()}
%%    Result = term()
%%    Reason = status_code() | term()
%%
%% @doc Sends an XMLRPC HTTP-request
%% @end
call(IP, Port, URI, ReqBody, Options) ->
    Host = concat_host(IP, Port),
    case post(Host, URI, ReqBody, Options) of
        {ok, {{_Ver, ?HTTP_OK, _Reason}, _Headers, RespBody}} ->
            {ok, RespBody};
        {ok, {{_Ver, _Code, Reason}, _Headers, _RespBody}} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.


%% @private
post(Host, URI, Body, Options) ->
    {Header, HttpOpts} = split_options(Options),
    http:request(post,
                 http_request(Host, URI, Header, Body),
                 HttpOpts,
                 []).


%% @private
http_request(Host, URI, Header, Body) ->
    {http_uri(Host, URI),
     http_headers(Host, Header, Body),
     [?CONTENT_TYPE],
     Body}.


%% @private
http_headers(Host, Header, Body) ->
    Headers = [{"Content-Length", integer_to_list(httpd_util:flatlength(Body))},
               {"Connection", "close"}, 
               {"User-Agent", ?USER_AGENT}, 
               {"Host", Host}],
    case proplists:get_value(auth, Header) of
        undefined ->
            Headers;
        {User, Pass} ->
            [basic_auth(User, Pass) | Headers]
    end.


%% @private
http_uri(Host, URI) ->
    lists:concat(["http://", Host, URI]).


%% @private
basic_auth(User, Pass) when is_list(User) and is_list(Pass) ->
    {"Authorization", "Basic " ++ base64:encode_to_string(User ++ ":" ++ Pass)}.


%% @private
concat_host({A, B, C, D}, Port) ->
    lists:concat([A,".",B,".",C,".",D,":",Port]).


%% @private
split_options(L) ->
    split_options(L, [], []).
split_options([], Header, Http) ->
    {Header, Http};
split_options([{auth, _} = H | T], Header, Http) ->
    split_options(T, [H | Header], Http);
split_options([H | T], Header, Http) ->
    split_options(T, Header, [H | Http]).
