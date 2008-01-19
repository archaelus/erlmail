%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       ERML commands for ErlMail
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
-module(erml_erlmail).
-author('sjackson@simpleenigma.com').
-include("../include/mime.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("erlweb/include/erml.hrl").

-behavior(gen_erml).

-export([ermlimap/2,ermlpop/2,ermlsmtp/2]).

ermlimap(_Element,ERML) -> {#xmlElement{name=ermlcommand},ERML}.
ermlpop(_Element,ERML)  -> {#xmlElement{name=ermlcommand},ERML}.









ermlsmtp(Element,ERML) -> 
	Required = gen_erml:get_attrib(Element,ERML,[server,to,from,subject]),
	Optional = gen_erml:get_attrib(Element,ERML,[]),
	Args = lists:append([Required,Optional]),
	{Message,_NewERML} = gen_erml:get_content(Element,ERML),
	IP = server_ip(gen_erml:get_value(server,Args)),
	From    = gen_erml:get_value(from,Args),
	Subject = gen_erml:get_value(subject,Args),
	To      = gen_erml:get_value(to,Args),
	MIME = #mime{header=[{from,From},{to,[To]},{subject,Subject}],body = string:strip(Message,both,10)},
	smtpc:sendmail(IP,25,"ErlMail",From,To,mime:encode(MIME)),
	{#xmlElement{name=ermlcommand},ERML}.





server_ip(Server) ->
	case Server of
		Server when is_tuple; size(Server) == 4 -> Server;
		Server ->
			case regexp:match(Server,"([0-9])+\.([0-9])+\.([0-9])+\.([0-9])+") of
				{match,_,_} ->
					[A1,A2,A3,A4] = string:tokens(Server,"."),
					{list_to_integer(A1),list_to_integer(A2),list_to_integer(A3),list_to_integer(A4)};
				nomatch ->
					case inet:gethostbyname(Server) of
						{ok,{hostent,_,_,inet,4,IPList}} ->
							lists:nth(1,IPList);
						_ -> {error,unknown_server_name}
					end
			end
	end.


