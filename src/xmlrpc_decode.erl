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
-module(xmlrpc_decode).


%%------------------------------------------------------------------------------
%% EXPORT
%%------------------------------------------------------------------------------
-export([do/1, do/2]).


%%------------------------------------------------------------------------------
%% INCLUDE
%%------------------------------------------------------------------------------
-include("gen_xmlrpc.hrl").
-include_lib("xmerl/include/xmerl.hrl").


%%------------------------------------------------------------------------------
%% MACROS
%%------------------------------------------------------------------------------
-define(SPACE, " \n\r\t").


%%------------------------------------------------------------------------------
%% EXPORT FUNCTIONS
%%------------------------------------------------------------------------------
%% @spec do(String) -> Result
%%     String = xmlString()
%%     Result = {methodCall, MethodName, Params} |
%%              {methodResponse, Params} |
%%              {methodResponse, {fault, FaultStruct}}
%%     MethodName = atom()
%%     Params = list()
%%      
%%
%% @doc Decodes XML <u>String</u> into Erlang term.
%% @end
do(String) ->
    do(String, []).


%% @spec do(String, Specs) -> term()
%%     String = xmlString()
%%     Spec = specList() | fun()
%%
%% @doc Decodes XML <u>String</u> into Erlang term. Validates XML according to 
%% <u>Specs</u>. Specs can be a list of #param{} or a fun/1 that returns list of
%% #param{} when passed the <u>methodCall</u> name. 
%% @end
do(String, Specs) when is_list(Specs); is_function(Specs, 1) ->
    {Doc, []} = xmerl_scan:string(String),
    parse(Doc, Specs).


%%------------------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%------------------------------------------------------------------------------
%% @private
parse(#xmlElement{name = methodCall, content = Content}, Specs) 
  when Content /= [], is_function(Specs, 1) ->
    MethodName = method_name(Content),
    {methodCall, MethodName, call_params(Content, Specs(MethodName))};
parse(#xmlElement{name = methodCall, content = Content}, Specs) 
  when Content /= [] ->
    {methodCall, method_name(Content), call_params(Content, Specs)};
parse(#xmlElement{name = methodResponse, content = Content}, Specs) 
  when Content /= [] ->
    {methodResponse, resp_params(Content, Specs)};
parse(#xmlElement{name = Name}, _) ->
    exit({error, {unexpected, Name}}).


%% @private
method_name([#xmlElement{name = methodName, content = Content} | _]) ->
    text_content(Content);
method_name([#xmlElement{name = params} | Rest]) ->
    method_name(Rest);
method_name([#xmlText{} | Rest]) ->
    method_name(Rest);
method_name([#xmlElement{name = Name} | _]) ->
    exit({error, {unexpected, Name}}).


%% @private
call_params([], []) ->
    [];
call_params([], _) ->
    exit({error, {missing_element, params}});
call_params([#xmlElement{name = params, content = Content} | _], Specs) ->
    param(Content, Specs);
call_params([#xmlElement{name = methodName} | Rest], Specs) ->
    call_params(Rest, Specs);
call_params([#xmlText{} | Rest], Specs) ->
    call_params(Rest, Specs);
call_params([#xmlElement{name = Name} | _], _) ->
    exit({error, {unexpected, Name}}).


%% @private
resp_params([#xmlElement{name = params, content = Content} | _], Specs) ->
    param(Content, Specs);
resp_params([#xmlElement{name = fault, content = Content} | _], _) ->
    {fault, fault_struct(match_elem(match_elem(Content, value), struct))};
resp_params([#xmlText{} | Rest], Specs) ->
    resp_params(Rest, Specs);
resp_params([#xmlElement{name = Name} | _], _) ->
    exit({error, {unexpected, Name}}).


%% @private
fault_struct(Content) ->
    fault_struct(Content, []).
fault_struct([], Acc) when length(Acc) == 2 ->
    Acc;
fault_struct([#xmlElement{name = member, content = Content} | Rest], Acc) 
  when length(Acc) == 0; length(Acc) == 1 ->
    case member_name(Content) of
        faultCode ->
            FaultCode = xmltype(member_value(Content)),
            case is_integer(FaultCode) of 
                true -> 
                    fault_struct(Rest, [{faultCode, FaultCode} | Acc]);
                false -> 
                    exit({error, {invalid_faultcode, FaultCode}})
            end;
        faultString ->
            FaultString = xmltype(member_value(Content)),
            case io_lib:printable_list(FaultString) of 
                true -> 
                    fault_struct(Rest, [{faultString, FaultString} | Acc]);
                false -> 
                    exit({error, {invalid_faultstring, FaultString}})
            end;
        Other ->
            exit({error, {unexpected_faultmember, Other}})
    end;
fault_struct([#xmlText{} | Rest], Acc) ->
    fault_struct(Rest, Acc);
fault_struct(_, _) ->
    exit({error, invalid_faultstruct}).


%% @private
param([], _) ->
    [];
param([#xmlElement{name = param, content = Content} | Rest], []) ->
    Value = match_elem(Content, value),
    [xmltype(Value) | param(Rest, [])];
param([#xmlElement{name = param, content = Content} | Rest], [#param{value = ParamSpec} | Specs]) ->
    Value = match_elem(Content, value),
    [xmltype(Value, ParamSpec) | param(Rest, Specs)];
param([#xmlText{} | Rest], Specs) ->
    param(Rest, Specs);
param([#xmlElement{name = Name} | _], _) ->
    exit({error, {unexpected_param, Name}}).


%% @private
member_name([#xmlElement{name = name, content = Content} | _]) ->
    text_content(Content);
member_name([#xmlElement{name = value} | Rest]) ->
    member_name(Rest);
member_name([#xmlText{} | Rest]) ->
    member_name(Rest);
member_name([#xmlElement{name = Name} | _]) ->
    exit({error, {unexpected_membername, Name}}).


%% @private
member_value([#xmlElement{name = value, content = Content} | _]) ->
    Content;
member_value([#xmlElement{name = name} | Rest]) ->
    member_value(Rest);
member_value([#xmlText{} | Rest]) ->
    member_value(Rest);
member_value([#xmlElement{name = Name} | _]) ->
    exit({error, {unexpected_membervalue, Name}}).


%% @private
xmltype(Content) ->
    xmltype(Content, []).
xmltype([], Spec) ->
    if 
        element(#string.rule, Spec) == mandatory ->
            exit({error, {missing_element, Spec}});
        true -> 
            []
    end;
xmltype([#xmlElement{name = array, content = Content} | _], Spec) ->
    case Spec of
        [] ->
            {array, array_members(match_elem(Content, data), [])};
        #array{value = ArrSpec} ->
            {array, array_members(match_elem(Content, data), ArrSpec)};
        Type ->
            exit({error, {wrong_type, Type}})
    end;
xmltype([#xmlElement{name = struct, content = Content} | _], Spec) ->
    case Spec of
        [] ->
            {struct, struct_members(Content, [])};
        #struct{members = StructSpec} ->
            {struct, struct_members(Content, StructSpec)};
        Type ->
            exit({error, {wrong_type, Type}})
    end;
xmltype([#xmlElement{name = Name, content = Content} | _], Spec) 
  when Name == int; Name == i4 ->
    case Spec of 
        [] ->
            int(Content);
        #int{} ->
            int(Content);
        Type ->
            exit({error, {wrong_type, Type}})
    end;
xmltype([#xmlElement{name = double, content = Content} | _], Spec) ->
    case Spec of 
        [] ->
            double(Content);
        #double{} ->
            double(Content);
        Type ->
            exit({error, {wrong_type, Type}})
    end;
xmltype([#xmlElement{name = boolean, content = Content} | _], Spec) ->
    case Spec of 
        [] ->
            boolean(Content);
        #boolean{} ->
            boolean(Content);
        Type ->
            exit({error, {wrong_type, Type}})
    end;
xmltype([#xmlElement{name = base64, content = Content} | _], Spec) ->
    case Spec of 
        [] ->
            base64(Content);
        #base64{} ->
            base64(Content);
        Type ->
            exit({error, {wrong_type, Type}})
    end;
xmltype([#xmlElement{name = string, content = Content} | _], Spec) ->
    case Spec of 
        [] ->
            string(Content);
        #string{} ->
            string(Content);
        Type ->
            exit({error, {wrong_type, Type}})
    end;
xmltype([#xmlElement{name = dateTime.iso8601, content = Content} | _], Spec) ->
    case Spec of 
        [] ->
            date(Content);
        #date{} ->
            date(Content);
        Type ->
            exit({error, {wrong_type, Type}})
    end;
xmltype([#xmlText{value = Text} | Rest], Spec) ->
    case {strip(Text, ?SPACE), Spec} of 
        {[], _} -> 
            xmltype(Rest, Spec);
        {String, []} -> 
            case io_lib:printable_list(String) of
                true ->
                    String;
                false ->
                    exit({error, {invalid_string, String}})
            end;
        {String, #string{}} ->
            case io_lib:printable_list(String) of
                true ->
                    String;
                false ->
                    exit({error, {invalid_string, String}})
            end;
        {_, Type} ->
            exit({error, {wrong_type, Type}})
    end;
xmltype([#xmlElement{name = Name} | _], Spec) ->
    case Spec of 
        [] ->
            exit({error, {unexpected_type, Name}});
        Type ->
            exit({error, {wrong_type, Type}})
    end.


%% @private
struct_members([], Specs) ->
    case lists:keysearch(mandatory, #string.rule, Specs) of
        false -> [];
        {value, Type} ->
            exit({error, {missing_element, Type}})
    end;
struct_members([#xmlElement{name = member, content = Content} | Rest], Specs) ->
    Name = member_name(Content),
    {MemberSpec, NewSpecs} = 
        case lists:keytake(Name, #string.name, Specs) of
            false -> {[], Specs};
            {value, Value, Specs2} -> {Value, Specs2}
        end,
    [{Name, xmltype(member_value(Content), MemberSpec)} | struct_members(Rest, NewSpecs)];
struct_members([#xmlText{} | Rest], Specs) ->
    struct_members(Rest, Specs);
struct_members([#xmlElement{name = Name} | _], _) ->
    exit({error, {unexpected_structmember, Name}}).


%% @private
array_members([], _) ->
    [];
array_members([#xmlElement{name = value, content = Content} | Rest], Spec) ->
    [xmltype(Content, Spec) | array_members(Rest, Spec)];
array_members([#xmlText{} | Rest], Acc) ->
    array_members(Rest, Acc);
array_members([#xmlElement{name = Name} | _], _) ->
    exit({error, {unexpected_arrayvalue, Name}}).


%% @private
date([#xmlText{value = Value} | Rest]) ->
    case strip(Value, ?SPACE) of
        [] ->
            date(Rest);
        DateTime ->
            case catch parse_date(DateTime) of
                {'EXIT', _} -> 
                    exit({error, {invalid_date, DateTime}});
                Date ->
                    Date
            end
    end.


%% @private
int([#xmlText{value = Value} | Rest]) ->
    case strip(Value, ?SPACE) of
        [] ->
            int(Rest);
        Int ->
            case string:to_integer(Int) of
                {Result, []} ->
                    Result;
                _Other ->
                    exit({error, {invalid_integer, Int}})
            end
    end.


%% @private
double([#xmlText{value = Value} | Rest]) ->
    case strip(Value, ?SPACE) of
        [] ->
            double(Rest);
        Double ->
            case string:to_float(Double) of
                {Result, []} -> 
                    Result;
                _Other ->
                    exit({error, {invalid_double, Double}})
            end
    end.


%% @private
boolean([#xmlText{value = Value} | Rest]) ->
    case strip(Value, ?SPACE) of
        [] ->
            boolean(Rest);
        "0" ->
            false;
        "1" ->
            true
    end.


%% @private
base64([#xmlText{value = Value} | Rest]) ->
    case strip(Value, ?SPACE) of
        [] ->
            base64(Rest);
        Base64 ->
            {base64, Base64}
    end.


%% @private
string([#xmlText{value = Value} | Rest]) ->
    case strip(Value, ?SPACE) of
        [] ->
            string(Rest);
        String ->
            case io_lib:printable_list(String) of
                true ->
                    String;
                false ->
                    exit({error, {invalid_string, String}})
            end
    end.


%% @private
match_elem([#xmlElement{name = Name, content = Content} | _], Name) ->
    Content;
match_elem([#xmlText{} | Rest], Acc) ->
    match_elem(Rest, Acc);
match_elem([#xmlElement{name = Name} | _], _) ->
    exit({error, {unexpected_element, Name}}).


%% @private
strip(String, SeparatorList) ->
    strip(String, both, SeparatorList).
strip(String, left, SeparatorList) ->
    lists:dropwhile(fun(X) -> lists:member(X, SeparatorList) end, String);
strip(String, right, SeparatorList) ->
    lists:reverse(strip(lists:reverse(String), left, SeparatorList));
strip(String, both, SeparatorList) ->
    strip(strip(String, left, SeparatorList), right, SeparatorList).


%% @private
text_content(XmlTextList) ->
    text_content(XmlTextList, []).
text_content([#xmlText{value = Value} | Rest], Acc) ->
    case strip(Value, ?SPACE) of
        [] ->
            text_content(Rest, Acc);
        String ->
            case io_lib:printable_list(String) of
                true ->
                    list_to_atom(String);
                false ->
                    exit({error, {invalid_string, String}})
            end
    end.    


%% @private
parse_date(DateTime) ->
    [Date, Time | _] = string:tokens(DateTime, "T+-"),
    Date2 = case string:tokens(Date, "-") of
                [Year, Month, Day] -> 
                    {list_to_integer(Year), 
                     list_to_integer(Month), 
                     list_to_integer(Day)};
                [[Y1,Y2,Y3,Y4,M1,M2,D1,D2]] ->
                    {list_to_integer([Y1, Y2, Y3, Y4]),
                     list_to_integer([M1, M2]),
                     list_to_integer([D1, D2])}
            end,
    Time2 = case string:tokens(Time, ":") of
                [Hour, Min, Sec] ->
                    {list_to_integer(Hour), 
                     list_to_integer(Min), 
                     list_to_integer(Sec)};
                [[H1,H2,MM1,MM2,S1,S2]] ->
                    {list_to_integer([H1, H2]), 
                     list_to_integer([MM1, MM2]), 
                     list_to_integer([S1, S2])}
            end,
    erlang:localtime_to_universaltime({Date2, Time2}),
    {Date2, Time2}.
