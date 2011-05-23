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
%% @doc XMLRPC handler
%%
%% @copyright 2011
%% @author Ali Yakout <ali.yakout@gmail.com>
%% @end
-module(mod_xmlrpc).


%%------------------------------------------------------------------------------
%% EXPORTS
%%------------------------------------------------------------------------------
-export([do/1]).

%%------------------------------------------------------------------------------
%% INCLUDE
%%------------------------------------------------------------------------------
-include_lib("inets/src/http_server/httpd.hrl").
-include("gen_xmlrpc.hrl").


%%------------------------------------------------------------------------------
%% EXPORT FUNCTIONS
%%------------------------------------------------------------------------------
%% @spec do(ModData) -> 
%%     {proceed, OldData} | {proceed, NewData} | {break, NewData} | done
%% 
%% @doc When a valid request reaches httpd it calls do/1 defined by the Modules 
%% configuration option. The field data in ModData is a list. This list will be 
%% the list returned from the last call to do/1
%% @end
do(#mod{method = "POST"} = Info) ->
    case proplists:get_value(status, Info#mod.data) of
        {_StatusCode, _PhraseArgs, _Reason} ->
            {proceed, Info#mod.data};
        undefined ->
            case proplists:get_value(response, Info#mod.data) of
                undefined ->
                    request(Info);
                _Response ->
                    {proceed, Info#mod.data}
            end
    end;
do(Info) ->
    Info.


%% @private
request(Info) ->
    URI = httpd_util:lookup(Info#mod.config_db, uri),
    if 
        Info#mod.request_uri == URI -> 
            Callback = httpd_util:lookup(Info#mod.config_db, callback),
            Tid = httpd_util:lookup(Info#mod.config_db, tid),
            [{callback_st, State}] = ets:lookup(Tid, callback_st),
            F = fun(MethodName) -> Callback:request_specs(MethodName, State) end,
            case catch xmlrpc_decode:do(Info#mod.entity_body, F) of
                {methodCall, MethodName, Params} ->
                    try Callback:handle_request(MethodName, Params, State) of
                        {reply, Reply, State} ->
                            response(Info, xmlrpc_encode:do({methodResponse, Reply}));
                        {reply, Reply, NewState} ->
                            ets:insert(Tid, {callback_st, NewState}),
                            response(Info, xmlrpc_encode:do({methodResponse, Reply}));
                        {stop, Reason, NewState} ->
                            catch Callback:terminate(Reason, NewState),
                            [{srv_ref, Pid}] = ets:lookup(Tid, srv_ref),
                            inets:stop(httpd, Pid),
                            done
                    catch
                        _:Exception ->
                            response(Info, io_lib:format("~p", [Exception]))
                    end;
                {'EXIT', Error} ->
                    response(Info, io_lib:format("~p", [Error]));
                _Other ->
                    response(Info, "Unknown request")
            end;
        true ->
            response(Info, "Invalid URI")
    end.


%% @private
response(Info, Data) ->
    Head=[{content_type, "text/xml"},
          {content_length, integer_to_list(httpd_util:flatlength(Data))},
          {code, ?HTTP_OK}],
    {proceed, [{response,{response, Head, Data}} | Info#mod.data]}.
