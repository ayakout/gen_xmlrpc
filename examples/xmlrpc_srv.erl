-module(xmlrpc_srv).


%%------------------------------------------------------------------------------
%% BEHAVIOUR
%%------------------------------------------------------------------------------
-behaviour(gen_xmlrpc).


%%------------------------------------------------------------------------------
%% INCLUDE
%%------------------------------------------------------------------------------
-include("gen_xmlrpc.hrl").


%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------
-export([start_link/3]).


%%------------------------------------------------------------------------------
%% gen_xmlrpc callbacks
%%------------------------------------------------------------------------------
-export([init/1, terminate/2,
         handle_request/3, request_specs/2]).


%%------------------------------------------------------------------------------
%% RECORDS
%%------------------------------------------------------------------------------
-record(state, {}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Port, URI, Options) -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Port, URI, Options) ->
    gen_xmlrpc:start_link(?MODULE, [], [{port, Port}, {uri, URI} | Options]).


%%%===================================================================
%%% gen_xmlrpc callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_xmlrpc when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_xmlrpc terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("Terminating ...~n"),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling XMLRPC messages
%%
%% @spec handle_request(MethodName, Params, State) ->
%%                                   {reply, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_request('GetServiceInfo', Params, State) ->
    io:format("~p~n", [Params]),
    Reply = [{struct,[{originTransactionID,"566613"},
                      {responseCode,0},
                      {serviceCurrent,5},
                      {serviceList,{array,[1,2,3,4]}}]}],
    {reply, Reply, State};
handle_request(_,_,State) ->
    Reply = {fault,[{faultCode, 4}, 
                    {faultString, "Too many parameters."}]},
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns XMLRPC specification
%%
%% @spec request_specs(MethodName, State) -> Result
%% @end
%%--------------------------------------------------------------------
request_specs('GetServiceInfo', _State) ->
    [#param{value = 
                #struct{members = 
                            [#string{name = originNodeType},
                             #string{name = originHostName},
                             #string{name = originTransactionID},
                             #string{name = subscriberNumber},
                             #date{name = originTimeStamp}]}}];
request_specs(_, _) ->
    [].


%%%===================================================================
%%% Internal functions
%%%===================================================================
