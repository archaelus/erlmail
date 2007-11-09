%%%---------------------------------------------------------------------------------------
%%% @author     Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc        Multipurpose Internet Mail Extention functions
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version    0.0.6
%%% @since      0.0.6
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Stuart Jackson, Simple Enigma, Inc. All Righs Reserved
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%
%%%---------------------------------------------------------------------------------------
-module(mime).
-author('sjackson@simpleenigma.com').
-include("../include/mime.hrl").

-export([encode/1,decode/1]).

-export([split/1,headers/1]).



%%-------------------------------------------------------------------------
%% @spec (Mime::mime()) -> string() | {error,Reason::atom()}
%% @type mime() = {mime,header,header_text,body,body_text}
%% @doc Encodes a #mime{} record into a string
%% @end
%%-------------------------------------------------------------------------
encode(_Message) -> ok.

%%-------------------------------------------------------------------------
%% @spec (Mime::mime()) -> mime() | {error,Reason::atom()}
%% @doc Recursively decodes a string into a #mime{} record.
%% @end
%%-------------------------------------------------------------------------
decode(Message) -> 
	M1 = split(Message),
	Headers = headers(M1#mime.header_text),
	M1#mime{header = Headers}.



%%-------------------------------------------------------------------------
%% @spec (HeaderText::string()) -> HeaderList::list()
%% @doc Parses HeaderText into a Key/Value list
%% @end
%%-------------------------------------------------------------------------
headers(HeaderText) ->
	{ok,H,_Lines} = regexp:gsub(HeaderText,";\n",";"),
	Tokens = string:tokens(H,[10]),
	headers(Tokens,[]).
%%-------------------------------------------------------------------------
%% @spec (list(),Acc::list()) -> list()
%% @hidden
%% @end
%%-------------------------------------------------------------------------
headers([H|T],Acc) ->
	Pos = string:chr(H,58),
	{HeaderString,Value} = lists:split(Pos,H),
	Header = list_to_atom(http_util:to_lower(string:strip(HeaderString,right,58))),
	headers(T,[{Header,string:strip(Value)}|Acc]);
headers([],Acc) -> lists:reverse(Acc).
	


%%-------------------------------------------------------------------------
%% @spec (Part::string()) -> mime() | {error,Reason::atom()}
%% @doc Splits the part at the lcoation of two LF (\n) in a row and 
%%      returns a #mime{} record. Performs some cleanup as well. 
%% @end
%%-------------------------------------------------------------------------
split(Part) ->
	Pos = string:str(Part,"\n\n"),
	{Header,Body} = lists:split(Pos+1,Part),
	#mime{header_text=string:strip(Header,right,10), body_text = Body}.























