%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       Multipurpose Internet Mail Extention functions
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.6
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

-export([split/1,headers/1,split_multipart/2]).



%%-------------------------------------------------------------------------
%% @spec (Mime::mime()) -> string() | {error,Reason::atom()}
%% @type mime() = {mime,header,header_text,body,body_text}
%% @doc Encodes a #mime{} record into a string
%% @end
%%-------------------------------------------------------------------------
encode(MIME) ->
	Header = enc_header(MIME#mime.header),
	Body = case MIME#mime.body of
		[#mime{}|_] = MIMEBody -> encode(MIMEBody);
		TextBody -> TextBody
	end,
	lists:flatten([Header,13,10,Body]).
	


enc_header(MIME) -> enc_header(MIME,[]).

enc_header([],Acc) -> lists:reverse(Acc);
enc_header([{from,From}|Rest],Acc) ->
	enc_header(Rest,[10,13,From,32,"From:"|Acc]);
enc_header([{to,To}|Rest],Acc) ->
	enc_header(Rest,[10,13,To,32,"To:"|Acc]);
enc_header([{subject,Subject}|Rest],Acc) ->
	enc_header(Rest,[10,13,Subject,32,"Subject:"|Acc]);
enc_header([{Atom,_Value}|Rest],Acc) ->
	enc_header(Rest,[10,13,58,atom_to_list(Atom)|Acc]).












%%-------------------------------------------------------------------------
%% @spec (Mime::mime()) -> mime() | {error,Reason::atom()}
%% @doc Recursively decodes a string into a #mime{} record.
%% @end
%%-------------------------------------------------------------------------
decode(Message) -> 
	MIME = split(Message),
	Headers = headers(MIME#mime.header_text),
	case lists:keysearch('content-type',1,Headers) of
		{value,{'content-type',Value}} ->
			case lists:prefix("multipart",http_util:to_lower(Value)) of
				true -> 
					Pos = string:chr(Value,61),
					{_,B} = lists:split(Pos,Value),
					Boundary = string:strip(B,both,34),
					Parts = split_multipart(Boundary,MIME#mime.body_text),
					MIMEParts = lists:map(fun(P) ->
							decode(P)
						end, Parts),
					MIME#mime{header = Headers, body = MIMEParts};
				false -> MIME#mime{header = Headers, body = MIME#mime.body_text}
			end;
		_ -> MIME#mime{header = Headers, body = MIME#mime.body_text}
	end.




%%-------------------------------------------------------------------------
%% @spec (HeaderText::string()) -> HeaderList::list()
%% @doc Parses HeaderText into a Key/Value list
%% @end
%%-------------------------------------------------------------------------
headers(HeaderText) ->
	{ok,H,_Lines} = regexp:gsub(HeaderText,"\r\n[\t ]"," "),
	Tokens = string:tokens(H,[13,10]),
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
	headers(T,[{Header,string:strip(Value)}|Acc]); %% @todo: strip tabs as well
headers([],Acc) -> lists:reverse(Acc).
	


%%-------------------------------------------------------------------------
%% @spec (Part::string()) -> mime() | {error,Reason::atom()}
%% @doc Splits the part at the lcoation of two CRLF (\r\n) in a row and 
%%      returns a #mime{} record. Performs some cleanup as well. Also checks 
%%      for two LF (\n) and splits on that as some bad messages for formed 
%%      this way.
%% @end
%%-------------------------------------------------------------------------
split(Part) ->
	case string:str(Part,?CRLF ++ ?CRLF) of
		0 ->
			case string:str(Part,"\n\n") of
				0 -> {error,no_break_found};
				Pos ->
					{Header,Body} = lists:split(Pos+1,Part),
					#mime{header_text=string:strip(Header,right,10), body_text = Body}
			end;
		Pos -> 
			{Header,Body} = lists:split(Pos+1,Part),
			#mime{header_text=string:strip(Header,right,10), body_text = Body}
	end.


%%-------------------------------------------------------------------------
%% @spec (Boundary::string(),Body::start()) -> Parts::list()
%% @doc Take the Body of a mutlipart MIME messages and split it into it's 
%%      parts on the boundary marks
%% @end
%%-------------------------------------------------------------------------
split_multipart(Boundary,Body) -> split_multipart(Boundary,Body,[]).
%%-------------------------------------------------------------------------
%% @spec (Boundary::string(),Body::start(),Acc::list()) -> Parts::list()
%% @hidden
%% @end
%%-------------------------------------------------------------------------
split_multipart(_Boundary,[],Acc) -> lists:reverse(Acc);
split_multipart(Boundary,Body,Acc) -> 
	case regexp:match(Body,Boundary) of
		{match,Start,Length} ->
			{_Pre,New} = lists:split(Start + Length + 1,Body),
			case regexp:match(New,Boundary) of
				{match,Start2,_Length2} ->
					{Part,Next} = lists:split(Start2 - 3,New),
					 split_multipart(Boundary,Next,[Part|Acc]);
				nomatch -> split_multipart(Boundary,[],Acc)
			end;
		nomatch -> split_multipart(Boundary,[],Acc)
	end.





















