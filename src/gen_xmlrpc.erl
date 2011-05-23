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
%% @doc XMLRPC generic behaviour
%%
%% @copyright 2011
%% @author Ali Yakout <ali.yakout@gmail.com>
%% @end
-module(gen_xmlrpc).


%%------------------------------------------------------------------------------
%% EXPORTS
%%------------------------------------------------------------------------------
-export([start_link/3, call/5, info/0]).


%%------------------------------------------------------------------------------
%% BEHAVIOUR EXPORTS
%%------------------------------------------------------------------------------
-export([behaviour_info/1]).


%%------------------------------------------------------------------------------
%% INCLUDE
%%------------------------------------------------------------------------------
-include("gen_xmlrpc.hrl").


%%------------------------------------------------------------------------------
%% BEHAVIOUR EXPORTS
%%------------------------------------------------------------------------------
%% @doc Behaviour callbacks
%% @end
behaviour_info(callbacks) ->
    [{init,1},
     {terminate, 2},
     {handle_request, 3},
     {request_specs, 2}];
behaviour_info(_Other) ->
    undefined.


%%------------------------------------------------------------------------------
%% START/STOP EXPORTS
%%------------------------------------------------------------------------------
%% @spec start_link(Module, Args, Options) -> {ok, Pid} | {error, Error}
%%     Module = atom()
%%     Args = term()
%%     Options = [Option]
%%     Option = {uri, uri()} | {users, list()} | InetsOpts
%%     InetsOpts = {port, int()} | {max_clients, int()}
%%     Error = {already_started,Pid} | term()
%%     Pid = pid()
%%
%% @doc Creates a gen_xmlrpc process. The gen_xmlrpc process calls 
%% Module:init/1 to initialize. 
%% Module is the name of the callback module. Args is an arbitrary term which 
%% is passed as the argument to Module:init/1.
%% 
%% <p> If the gen_xmlrpc is successfully created and initialized the function 
%% returns {ok,Pid} , where Pid is the pid of the gen_xmlrpc. If there already
%% exists a process with the specified ServerName the function returns 
%% {error,{already_started,Pid}} , where Pid is the pid of that process.</p>
%% 
%% <p> If  Module:init/1 fails with Reason , the function returns 
%% {error,Reason}. If Module:init/1 returns {stop,Reason} or ignore, the 
%% process is terminated and the function returns {error,Reason} or ignore, 
%% respectively.</p>
%%
%% <p><u>max_clients</u> limits the number of simultaneous requests that can be 
%% supported. Defaults to 150.</p>
%% @end
start_link(Mod, Args, Options)
  when is_atom(Mod), is_list(Args), is_list(Options) ->
    {XmlrpcOpts, InetsOpts} = split_options(Options),
    case catch Mod:init(Args) of
        {ok, ModState} ->
            URI = proplists:get_value(uri, XmlrpcOpts, "/"),
            Tid = ets:new(?MODULE, [public, set]),
            Users = proplists:get_value(users, Options, []),
            ServiceConfig = instance(atom_to_list(Mod), Users, 
                                     [{callback, Mod}, {uri, URI}, 
                                      {tid, Tid} | InetsOpts]),
            case inets:start(httpd, ServiceConfig, stand_alone) of
                {ok, Pid} ->
                    ets:insert(Tid, {callback_st, ModState}),
                    ets:insert(Tid, {srv_ref, Pid}),
                    {ok, Pid};
                {error, Reason} ->
                    ets:delete(Tid),
                    {error, Reason}
            end;                
        {stop, Reason} ->
            exit(Reason);
        ignore ->
            exit(normal);
        {'EXIT', Reason} ->
            exit(Reason);
        Else ->
            Error = {bad_return_value, Else},
            exit(Error)
    end.


%% @spec call(Host, Port, URI, ReqBody, Options) -> {ok, Result} | {error, Reason}
%%    Host = ip() | ipString()
%%    Port = integer()
%%    URI = string()
%%    ReqBody = {MethodName, MethodParams}
%%    MethodName = atom()
%%    MethodParams = term()
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
call(Host, Port, URI, ReqBody, Options)
  when is_list(Host), is_list(URI), is_integer(Port),
       is_tuple(ReqBody), is_list(Options) ->
    case inet_parse:address(Host) of
        {ok, IP} ->
            call(IP, Port, URI, ReqBody, Options);
        Error ->
            Error
    end;
call(Host, Port, URI, {MethodName, MethodParams}, Options)
  when is_tuple(Host), is_list(URI), is_integer(Port), is_list(Options) ->
    ReqBody = {methodCall, MethodName, MethodParams},
    case catch xmlrpc_http:call(Host, Port, URI, lists:flatten(xmlrpc_encode:do(ReqBody)), Options) of
        {'EXIT', Error} ->        
            {error, Error};
        {ok, Result} ->
            case catch xmlrpc_decode:do(Result) of
                {'EXIT', Error2} ->
                    {error, Error2};
                {methodResponse, Response} ->
                    Response
            end
    end.


%% @spec info() -> [{Service, Pid, Info}]
%%     Service = service()
%%     Pid = pid()
%%     Info = [{Option, Value}]
%%     Option = property()
%%     Value = term()
%%
%% @doc Returns a list of currently running services.
%% @end
info() ->
    inets:services_info().


%% @private
instance(ServerName, Users, Options) ->
    PrivDir = code:priv_dir(?MODULE),
    Config = [{server_name, ServerName},
               {server_root, PrivDir},
               {document_root, PrivDir},
               {modules, [mod_auth, mod_xmlrpc]},
               {mime_types, [{".xml", "text/xml"}]} | Options],
    if 
        Users /= [] ->
            UserFile = filename:join(PrivDir, "users.txt"),
            GroupFile = filename:join(PrivDir, "groups.txt"),
            [{directory, {[],[{auth_type,plain}, 
                              {auth_name, ServerName},
                              {auth_user_file, UserFile},
                              {auth_group_file, GroupFile},
                              {require_user, Users}]}} | Config];
        true ->
            Config
    end.


%% @private
split_options(L) ->
    split_options(L, [], []).
split_options([], Xmlrpc, Inets) ->
    {Xmlrpc, Inets};
split_options([{uri, _} = H | T], Xmlrpc, Inets) ->
    split_options(T, [H | Xmlrpc], Inets);
split_options([{users, _} = H | T], Xmlrpc, Inets) ->
    split_options(T, [H | Xmlrpc], Inets);
split_options([H | T], Xmlrpc, Inets) ->
    split_options(T, Xmlrpc, [H | Inets]).
