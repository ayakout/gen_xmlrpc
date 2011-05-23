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


%%------------------------------------------------------------------------------
%% RECORDS
%%------------------------------------------------------------------------------
-record(param, {rule = mandatory, value}).
-record(struct, {name = param, rule = mandatory, members = []}).
-record(array, {name = param, rule = mandatory, value}).
-record(string, {name, rule = mandatory}).
-record(date, {name, rule = mandatory}).
-record(int, {name, rule = mandatory}).
-record(boolean, {name, rule = mandatory}).
-record(double, {name, rule = mandatory}).
-record(base64, {name, rule = mandatory}).


%%------------------------------------------------------------------------------
%% MACROS
%%------------------------------------------------------------------------------
-define(CONTENT_TYPE, "text/xml; charset=utf-8").
-define(USER_AGENT, "Erlang XMLRPC/INETS").
-define(HTTP_OK, 200).
-define(XML_HEADER, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").
-define(METHOD_CALL(Name, Params), 
        "<methodCall>"
        "<methodName>", Name, "</methodName>"
        "<params>", 
        [["<param><value>", P, "</value></param>"] || P <- Params], 
        "</params>"
        "</methodCall>").
-define(METHOD_RESP(Params),
        "<methodResponse>"
        "<params>",
        [["<param><value>", P, "</value></param>"] || P <- Params],
        "</params>"
        "</methodResponse>").
-define(FAULT(Code, String), 
        "<methodResponse>"
        "<fault>"
        "<value>"
        "<struct>"
        "<member>"
        "<name>faultCode</name>"
        "<value><int>", integer_to_list(Code), "</int></value>"
        "</member>"
        "<member>"
        "<name>faultString</name>"
        "<value><string>", String, "</string></value>"
        "</member>"
        "</struct>"
        "</value>"
        "</fault>"
        "</methodResponse>").
