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
%% @doc XMLRPC encoder
%%
%% @copyright 2011
%% @author Ali Yakout <ali.yakout@gmail.com>
%% @end
-module(xmlrpc_encode).


%%------------------------------------------------------------------------------
%% EXPORTS
%%------------------------------------------------------------------------------
-export([do/1]).


%%------------------------------------------------------------------------------
%% INCLUDE
%%------------------------------------------------------------------------------
-include("gen_xmlrpc.hrl").
-include_lib("xmerl/include/xmerl.hrl").


%%------------------------------------------------------------------------------
%% EXPORT FUNCTIONS
%%------------------------------------------------------------------------------
%% @spec do(XmlrpcTerm) -> io_list()
%%     XmlrpcTerm = {methodCall, MethodName, Params} |
%%                  {methodResponse, Params} |
%%                  {methodResponse, {fault, FaultStruct}}
%%     MethodName = atom()
%%     Params = term()
%%     FaultStruct = [FaultElement]
%%     FaultElement = {faultCode, FaultCode} | {faultString, FaultString}
%%     FaultCode = int()
%%     FaultString = string()
%%
%% @doc Converts Xmlrpc Erlang term to Xmlrpc io_list.
%% @end
do({methodCall, MethodName, Params}) -> 
    [?XML_HEADER, 
     ?METHOD_CALL(atom_to_list(MethodName),
                  [term_to_xml(P) || P <- Params])];
do({methodResponse, {fault, FaultStruct}}) ->
    Code = proplists:get_value(faultCode, FaultStruct),
    String = proplists:get_value(faultString, FaultStruct),
    if 
        is_list(String) andalso is_integer(Code) ->
            [?XML_HEADER, ?FAULT(Code, String)];
        true ->
            exit({error, invalid_faultstruct})
    end;
do({methodResponse, Params}) -> 
    [?XML_HEADER, ?METHOD_RESP([term_to_xml(P) || P <- Params])].


%% @private
term_to_xml({array, Value}) ->
    ["<array><data>", 
     [["<value>", term_to_xml(V), "</value>"] || V <- Value], 
     "</data></array>"];
term_to_xml({struct, Value}) ->
    ["<struct>", 
     [["<member>", term_to_xml(V), "</member>"] || V <- Value],
     "</struct>"];
term_to_xml(Value) when is_list(Value) ->
    ["<string>", escape(Value), "</string>"];
term_to_xml(Value) when is_integer(Value) ->
    ["<int>", integer_to_list(Value), "</int>"];
term_to_xml(Value) when is_float(Value) ->
    ["<double>", io_lib:format("~f",[Value]), "</double>"];
term_to_xml({{Year, Month, Day}, {Hour, Min, Sec}}) ->    
    ["<dateTime.iso8601>",
     [integer_to_list(Year), 
      string:right(integer_to_list(Month), 2, $0), 
      string:right(integer_to_list(Day), 2, $0), "T", 
      string:right(integer_to_list(Hour), 2, $0), ":",
      string:right(integer_to_list(Min), 2, $0), ":",
      string:right(integer_to_list(Sec), 2, $0)],
     "</dateTime.iso8601>"];
term_to_xml({Key, Value}) ->
    ["<name>", atom_to_list(Key), "</name>", 
     "<value>", term_to_xml(Value), "</value>"];
term_to_xml(true) -> 
    "<boolean>1</boolean>";
term_to_xml(false) -> 
    "<boolean>0</boolean>";
term_to_xml(Element) -> 
    exit({error, {invalid_element, Element}}).


%% @private
escape(String) ->
    escape(String, []).
escape([], Acc) ->
    lists:reverse(Acc);
escape([$< | T], Acc) ->
    escape(T, ["&lt" | Acc]);
escape([$> | T], Acc) ->
    escape(T, ["&gt" | Acc]);
escape([$& | T], Acc) ->
    escape(T, ["&amp" | Acc]);
escape([H | T], Acc) ->
    escape(T, [H | Acc]).
