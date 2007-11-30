%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       IMAP server response processing
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
-module(imapd_resp).
-author('sjackson@simpleenigma.com').
-include("../include/imap.hrl").
-include("../include/erlmail.hrl").

-behaviour(gen_server).

%% External API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
% Mnesia control functions
-export([create/0,join/0,exit/0,check/0]).





start_link(_,_) -> ok.
code_change(_,_,_) -> ok.
handle_call(_,_,_) -> ok.
handle_cast(_,_) -> ok.
handle_info(_,_) -> ok.
init(_) -> 
	% check for mnesia tables nad see if max tables limit has been met.
	% if not ables had been created initalize it
	% if max is not met copy table to node.
	ok.
terminate(_,_) -> 
	% remove mnesia table from self() is one is located on node
	ok.


% mnesia rotines
create() -> ok.
join() -> ok.
exit() -> ok.
check() -> ok.























