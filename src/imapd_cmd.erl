%%%---------------------------------------------------------------------------------------
%%% @author     Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright  2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc        IMAP server command processing
%%% @reference  See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
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
-module(imapd_cmd).
-author('sjackson@simpleenigma.com').
-include("../include/imap.hrl").
-include("../include/erlmail.hrl").

-export([command/1,command/2]).


%%-------------------------------------------------------------------------
%% @spec (State::imapd_fsm()) -> NewState::imapd_fsm()
%% @doc  Processes all IMAP commands and checks for extention processing
%% @end
%%-------------------------------------------------------------------------
command(#imapd_fsm{line = Line} = State) -> 
	Command = imapd_util:parse(Line),
	io:format("Command: ~p~n",[Command]),
	command(Command,State).


%%-------------------------------------------------------------------------
%% @spec (Comamnd::imap_cmd(),State::imapd_fsm()) -> NewState::imapd_fsm()
%% @hidden
%% @end
%%-------------------------------------------------------------------------
%%%-------------------------
%%% CAPABILITY - Any State
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = capability = Command, data = []}, State) -> 
	imapd_util:out(Command, State),
	Capability = imapd_ext:capability(),
	imapd_util:send(#imap_resp{tag = '*', cmd = capability, info = Capability}, State),
	imapd_util:send(#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"}, State),
	State;
command(#imap_cmd{tag = Tag, cmd = capability = Command, data = Data}, State) -> 
	imapd_util:out(Command, Data, State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad, info = "Protocol Error: Unexpected additional characters at the end of the command"}, State),
	State;

%%%-------------------------
%%% NOOP - Any State
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = noop = Command, data = []}, State) -> 
	imapd_util:out(Command, State),
	lists:map(fun(Response) -> 
		imapd_util:send(Response,State)
		end, State#imapd_fsm.responses),
	imapd_util:send(#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"},State),
	State#imapd_fsm{responses = []};
command(#imap_cmd{tag = Tag, cmd = noop = Command, data = Data}, State) -> 
	imapd_util:out(Command,Data,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad, info = "Protocol Error: Unexpected additional characters at the end of the command"},State),
	State;

%%%-------------------------
%%% LOGOUT - Any State
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = logout = Command, data = []}, State) -> 
	imapd_util:out(Command, State),
	imapd_util:send(#imap_resp{tag = '*', status = bye, info = "ErlMail terminating connection"}, State),
	imapd_util:send(#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"}, State),
	gen_tcp:close(State#imapd_fsm.socket),
	State;
command(#imap_cmd{tag = Tag, cmd = logout = Command, data = Data}, State) -> 
	imapd_util:out(Command, Data, State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad, info = "Protocol Error: Unexpected additional characters at the end of the command"},State),
	State;

%%%-------------------------
%%% STARTTLS - Not Authenticated
%%%-------------------------

% @todo STARTTLS impliment command


%%%-------------------------
%%% AUTHENTICATE - Not Authenticated
%%%-------------------------

% @todo AUTHENTICATE impliment command


%%%-------------------------
%%% LOGIN - Not Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = login = Command, data = []},  State) -> 
	imapd_util:out(Command, State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad, info = "Protocol Error: no login data"}, State),
	State;
command(#imap_cmd{tag = Tag, cmd = login = Command, data = {UserName,Password}}, 
	    #imapd_fsm{state = not_authenticated, options = Options} = State) -> 
	?D({UserName,Password}),
	case lists:keysearch(logindisabled, 1, Options) of
		{value, {logindisabled,true}} ->
			imapd_util:send(#imap_resp{tag = Tag, status = no, info = "failure: command disabled"},State),
			State;
		_ ->
			imapd_util:out(Command,UserName,State),
			Store = gen_store:lookup(user,State),
			User = Store:select(erlmail_util:split_email(UserName)),
			?D(User),
			case User of
				#user{password = Password} = User when Password /= [] -> 
					imapd_util:send(#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"},State),
					State#imapd_fsm{state = authenticated, user = User};
				_ -> 
					imapd_util:send(#imap_resp{tag = Tag, status = no, info = "failure: unknown user name or bad password"},State),
					State
			end
	end;
command(#imap_cmd{tag = Tag, cmd = login = Command},
		#imapd_fsm{state = FSMState} = State) when FSMState =:= selected; FSMState =:= authenticated -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = no, info = "Already logged in"},State),
	State;

%%%-------------------------
%%% SELECT - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = select = Command, data = []}, State) -> 
	imapd_util:out(Command, State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad, info = "Protocol Error: no mailbox data"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = select = Command}, 
	    #imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command, State),
	imapd_util:send(#imap_resp{tag = Tag, status = no, info = "Failure: not authenticated"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = select = Command, data = MailBoxName},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	imapd_util:out(Command, MailBoxName, State),
	Store = gen_store:lookup(mailbox_store, State),
	MailBoxStore = Store:select({MailBoxName, User#user.name}),
	case MailBoxStore of
		MailBoxStore when is_record(MailBoxStore, mailbox_store) ->
			MailBox = imapd_util:mailbox_info(MailBoxStore),
			imapd_util:send(#imap_resp{tag = "*", status = MailBox#mailbox.exists, cmd = exists},State),
			imapd_util:send(#imap_resp{tag = "*", status = MailBox#mailbox.recent, cmd = recent},State),
			% @todo Clean up Flag processing - figure out where to store data
			imapd_util:send(#imap_resp{tag = "*", cmd = flags, data = {flags,MailBox#mailbox.flags}},State),
			imapd_util:send(#imap_resp{tag = "*", status = ok, code = {unseen,MailBox#mailbox.unseen}},State),
			imapd_util:send(#imap_resp{tag = "*", status = ok, code = {uidvalidity,MailBoxStore#mailbox_store.uidvalidity}},State),
			imapd_util:send(#imap_resp{tag = "*", status = ok, code = {uidnext,MailBoxStore#mailbox_store.uidnext}},State),
			% @todo Clean up PermanentFlag processing - figure out where to store data
			imapd_util:send(#imap_resp{tag = "*", status = ok, code = {permanentflags,[answered,flagged,draft,deleted,seen,'*']}},State),
			imapd_util:send(#imap_resp{tag = Tag, status = ok, code = 'read-write', cmd = Command, info = "Completed"},State),
			State#imapd_fsm{state = selected, mailbox = MailBoxStore};
		_ ->
			imapd_util:send(#imap_resp{tag = Tag, status = no, cmd = Command, info = "failure: no such mailbox"},State),
			State#imapd_fsm{state = authenticated, mailbox = []}
	end;
%%%-------------------------
%%% EXAMINE - Authenticated
%%%-------------------------

command(#imap_cmd{tag = Tag, cmd = examine = Command, data = []}, State) -> 
	imapd_util:out(Command, State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad, info = "Protocol Error: no mailbox data"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = examine = Command}, 
	    #imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command, State),
	imapd_util:send(#imap_resp{tag = Tag, status = no, info = "Failure: not authenticated"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = examine = Command, data = MailBoxName},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	imapd_util:out(Command, MailBoxName, State),
	Store = gen_store:lookup(mailbox_store, State),
	MailBoxStore = Store:select({MailBoxName, User#user.name}),
	case MailBoxStore of
		MailBoxStore when is_record(MailBoxStore, mailbox_store) ->
			MailBox = imapd_util:mailbox_info(MailBoxStore),
			imapd_util:send(#imap_resp{tag = "*", status = MailBox#mailbox.exists, cmd = exists},State),
			imapd_util:send(#imap_resp{tag = "*", status = MailBox#mailbox.recent, cmd = recent},State),
			% @todo Clean up Flag processing - figure out where to store data
			imapd_util:send(#imap_resp{tag = "*", cmd = flags, data = {flags,MailBox#mailbox.flags}},State),
			imapd_util:send(#imap_resp{tag = "*", status = ok, code = {unseen,MailBox#mailbox.unseen}},State),
			imapd_util:send(#imap_resp{tag = "*", status = ok, code = {uidvalidity,MailBoxStore#mailbox_store.uidvalidity}},State),
			imapd_util:send(#imap_resp{tag = "*", status = ok, code = {uidnext,MailBoxStore#mailbox_store.uidnext}},State),
			% @todo Clean up PermanentFlag processing - figure out where to store data
			imapd_util:send(#imap_resp{tag = "*", status = ok, code = {permanentflags,[answered,flagged,draft,deleted,seen,'*']}},State),
			imapd_util:send(#imap_resp{tag = Tag, status = ok, code = 'read-only', cmd = Command, info = "Completed"},State),
			State#imapd_fsm{state = selected, mailbox = MailBoxStore};
		_ ->
			imapd_util:send(#imap_resp{tag = Tag, status = no, cmd = Command, info = "failure: no such mailbox"},State),
			State#imapd_fsm{state = authenticated, mailbox = []}
	end;
%%%-------------------------
%%% CREATE - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = create = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad, info = "Protocol Error: no mailbox name"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = create = Command},
	    #imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = no, info = "Failure: not authenticated"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = create = Command, data = Data},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	imapd_util:out(Command,Data,State),
	Store = gen_store:lookup(mailbox_store, State),
	case Store:select({Data,User#user.name}) of
		[] -> 
			% @todo CREATE Check for and clear trailing hierarchy seprator
			% @todo CREATE parent mailboxes
			% @todo CREATE UIDVALIDITY for mailbox
			% @todo CREATE check previosuly deleted forlder info for MAX UID
			{UserName,DomainName} = User#user.name,
			Store:insert(#mailbox_store{name={Data,UserName,DomainName}}),
			imapd_util:send(#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"},State);
		MailBoxStore when is_record(MailBoxStore,mailbox_store) -> 
			 imapd_util:send(#imap_resp{tag = Tag, status = no, info = "Failure: Mailbox Exists"},State);
		_ -> imapd_util:send(#imap_resp{tag = Tag, status = no, info = "Failure: cannot create mailbox"},State)
	end,
	State;
%%%-------------------------
%%% DELETE - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = delete = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad, info = "Protocol Error: no mailbox data"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = delete = Command},
	    #imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = no, info = "Failure: not authenticated"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = delete = Command, data = MailBoxName},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	imapd_util:out(Command,MailBoxName,State),
	Store = gen_store:lookup(mailbox_store, State),
	case Store:select({MailBoxName,User#user.name}) of
		[] ->  imapd_util:send(#imap_resp{tag = Tag, status = no, info = "Failure: Mailbox Does Not Exists"},State);
		#mailbox_store{name = {"INBOX",_,_}} -> 
			 imapd_util:send(#imap_resp{tag = Tag, status = no, info = "Failure: Cannot delete INBOX"},State);
		MailBoxStore when is_record(MailBoxStore,mailbox_store) -> 
			{UserName,DomainName} = User#user.name,
			% @todo DELETE messages and cleanup
			% @todo DELETE check for \noselect flag; error
			% @todo DELETE check for sub folders; remove mail and set \noselect leave folder
			% @todo DELETE maintain list of Max UID for deleted folders incase of recreation
			?D({delete,MailBoxName,UserName,DomainName}),
			Store:delete(#mailbox_store{name={MailBoxName,UserName,DomainName}}),
			imapd_util:send(#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"},State);
		_ -> imapd_util:send(#imap_resp{tag = Tag, status = no, info = "Failure: cannot create mailbox"},State)
	end,
	State;

%%%-------------------------
%%% RENAME - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = rename = Command, data = {[],[]}}, State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad, cmd = Command, info = "Protocol Error: no mailbox names"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = rename = Command, data = {_,[]}}, State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad, cmd = Command, info = "Protocol Error: no destination mailbox"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = rename = Command},
        #imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = no, cmd = Command, info = "Failure: not authenticated"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = rename = Command, data = {Src,Dst}},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	imapd_util:out(Command,{Src,Dst},State),
	Store = gen_store:lookup(mailbox_store, State),
	SrcMB = Store:select({string:strip(Src),User#user.name}),
	DstMB = Store:select({string:strip(Dst),User#user.name}),
	case {SrcMB,DstMB} of
		{[],_} -> imapd_util:send(#imap_resp{tag = Tag, status = no},State);
		{_,DstMB} when is_record(DstMB,mailbox_store) -> 
			imapd_util:send(#imap_resp{tag = Tag, status = no},State);
		{SrcMB,[]} ->
			% @todo RENAME any sub folders
			% @todo RENAME create any parent folders
			% @todo RENAME maintain list of Max UID for renamed folders incase of recreation
			% @todo RENAME INBOX special case. Move Mail, but do not delete INBOX. Leave subfolders alone
			{_,UserName,DomainName} = SrcMB#mailbox_store.name,
			NewMB = SrcMB#mailbox_store{name = {Dst,UserName,DomainName}},
			Store:insert(NewMB),
			Store:delete(SrcMB),
			imapd_util:send(#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"},State);
		_ -> imapd_util:send(#imap_resp{tag = Tag, status = no, cmd = Command},State)
	end,
	State;

%%%-------------------------
%%% SUBSCRIBE - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = subscribe = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad, info = "Protocol Error: no mailbox name"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = subscribe = Command},
	    #imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = no, info = "Failure: not authenticated"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = subscribe = Command, data = MailBoxName},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	imapd_util:out(Command,MailBoxName,State),
	Store = gen_store:lookup(mailbox_store, State),
	case Store:select({MailBoxName,User#user.name}) of
		[] -> imapd_util:send(#imap_resp{tag = Tag, status = no},State);
		MailBox when is_record(MailBox,mailbox_store) ->
			NewMailBox = MailBox#mailbox_store{subscribed = true},
			Store:update(NewMailBox),
			imapd_util:send(#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"},State);
		_ -> imapd_util:send(#imap_resp{tag = Tag, status = no},State)
	end,
	State;
%%%-------------------------
%%% UNSUBSCRIBE - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = unsubscribe = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad, info = "Protocol Error: no mailbox data"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = unsubscribe = Command},
	    #imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = no, info = "Failure: not authenticated"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = unsubscribe = Command, data = MailBoxName},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	imapd_util:out(Command,MailBoxName,State),
	Store = gen_store:lookup(mailbox_store, State),
	case Store:select({MailBoxName,User#user.name}) of
		[] ->imapd_util:send(#imap_resp{tag = Tag, status = no},State);
		MailBox when is_record(MailBox,mailbox_store) ->
			NewMailBox = MailBox#mailbox_store{subscribed = false},
			Store:update(NewMailBox),
			imapd_util:send(#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"},State);
		_ -> imapd_util:send(#imap_resp{tag = Tag, status = no},State)
	end,
	State;
%%%-------------------------
%%% LIST - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = list = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad, info = "Protocol Error"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = list = Command},
		#imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = no},State),
	State;
command(#imap_cmd{tag = Tag, cmd = list = Command, data = {_Reference,MailBoxName}},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	% @todo incorparete reference name. Simplier not to use at first
%	?D({Reference,MailBoxName}),
	Store = gen_store:lookup(mailbox_store, State),
	Heirachy = imapd_util:heirachy_char(),
%	?D(Heirachy),
	case Store:mlist(MailBoxName,User#user.name,false) of
		List when is_list(List) -> 
%			?D(List),
			lists:map(fun(Name) -> 
				Info = {Command,Heirachy,Name},
				Data = {Command,[]},
				imapd_util:send(#imap_resp{tag = '*', cmd = Command, data = Data, info = Info},State)
				end,List),
			imapd_util:send(#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"},State);
		undefined -> imapd_util:send(#imap_resp{tag = Tag, status = no},State);
		_ -> imapd_util:send(#imap_resp{tag = Tag, status = bad},State)
	end,
	State;


%%%-------------------------
%%% LSUB - Authenticated
%%%-------------------------

command(#imap_cmd{tag = Tag, cmd = lsub = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad, info = "Protocol Error"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = lsub = Command},
		#imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = no},State),
	State;
command(#imap_cmd{tag = Tag, cmd = lsub = Command, data = {Reference,MailBoxName}},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	% @todo incorparete reference name. Simplier not to use at first
	?D({Reference,MailBoxName}),
	Store = gen_store:lookup(mailbox_store, State),
	Heirachy = imapd_util:heirachy_char(),
	case Store:mlist(MailBoxName,User#user.name,true) of
		List when is_list(List) -> 
			lists:map(fun(Name) -> 
				Info = {Command,Heirachy,Name},
				Data = {Command,[]},
				imapd_util:send(#imap_resp{tag = '*', cmd = Command, data = Data, info = Info},State)
				end,List),
			imapd_util:send(#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"},State);
		undefined -> imapd_util:send(#imap_resp{tag = Tag, status = no},State);
		_ -> imapd_util:send(#imap_resp{tag = Tag, status = bad},State)
	end,
	State;

%%%-------------------------
%%% STATUS - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = status = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad, info = "Protocol Error: no mailbox data"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = status = Command},
		#imapd_fsm{state = not_authenticated} = State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = no},State),
	State;
command(#imap_cmd{tag = Tag, cmd = status = Command, data = {MailBoxName,Flags}},
		#imapd_fsm{state = FSMState, user = User} = State) when FSMState =:= authenticated; FSMState =:= selected -> 
	imapd_util:out(Command,MailBoxName,State),
	Store = gen_store:lookup(mailbox_store, State),
	case Store:select({string:strip(MailBoxName),User#user.name}) of
		[] ->
			imapd_util:send(#imap_resp{tag = Tag, status = no},State);
		MailBox when is_record(MailBox,mailbox_store) -> 
			StatusFlags = imapd_util:status_flags(Flags),
			MailBoxInfo = imapd_util:mailbox_info(MailBox,StatusFlags),
			StatusInfo  = imapd_util:status_info(MailBoxInfo,StatusFlags),
			% @todo Process each flag and build data to return
			Status = {status,MailBoxName,StatusInfo},
			imapd_util:send(#imap_resp{tag = '*', cmd = Command, data = Status},State),
			imapd_util:send(#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"},State);
		_ -> 
			imapd_util:send(#imap_resp{tag = Tag, status = no},State)
	
	end,

	State;

%%%-------------------------
%%% APPEND - Authenticated
%%%-------------------------

% @todo APPEND impliment command


%%%-------------------------
%%% CHECK - Selected
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = check = Command},
		#imapd_fsm{state = FSMState} = State) when FSMState =:= not_authenticated; FSMState =:= authenticated -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad},State),
	State;
command(#imap_cmd{tag = Tag, cmd = check = Command, data = []},#imapd_fsm{state = selected} = State) -> 
	imapd_util:out(Command,State),
	{Domain,User,Message,MailBox} = gen_store:lookup(all, State),
	Domain:check(domain),
	User:check(user),
	Message:check(message),
	MailBox:check(mailbox_store),
	imapd_util:send(#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = check = Command}, State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad},State),
	State;

%%%-------------------------
%%% CLOSE - Selected
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = close = Command},
		#imapd_fsm{state = FSMState} = State) when FSMState =:= not_authenticated; FSMState =:= authenticated -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad},State),
	State;
command(#imap_cmd{tag = Tag, cmd = close = Command, data = []},
		#imapd_fsm{state = selected} = State) -> 
	imapd_util:out(Command,State),
	% @todo CLOSE If read-write then expunge, else don't - need expunge funcation
	imapd_util:send(#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"},State),
	State#imapd_fsm{mailbox = [], state = authenticated};
command(#imap_cmd{tag = Tag, cmd = close = Command}, State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad},State),
	State;

%%%-------------------------
%%% EXPUNGE - Authenticated
%%%-------------------------

% @todo EXPUNGE impliment command

%%%-------------------------
%%% SEARCH - Authenticated
%%%-------------------------

% @todo SEARCH impliment command

%%%-------------------------
%%% FETCH - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = fetch = Command},
		#imapd_fsm{state = FSMState} = State) when FSMState =:= not_authenticated; FSMState =:= authenticated -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad},State),
	State;
command(#imap_cmd{tag = Tag, cmd = fetch = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad, info = "Protocol Error: no data"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = fetch = Command, data = {Seq,Data}}, State) -> 
	imapd_util:out(Command,State),
	
	?D({Seq,Data}),
	
	imapd_util:send(#imap_resp{tag = Tag, status = ok, cmd= Command, info = "Completed"},State),
	State;

% @todo FETCH impliment command

%%%-------------------------
%%% STORE - Selected
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = store = Command},
		#imapd_fsm{state = FSMState} = State) when FSMState =:= not_authenticated; FSMState =:= authenticated -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad},State),
	State;
command(#imap_cmd{tag = Tag, cmd = store = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad},State),
	State;
command(#imap_cmd{tag = Tag, cmd = store = Command, data = {Seq,Action,Flags}},#imapd_fsm{state = selected, mailbox = Selected} = State) -> 
	imapd_util:out(Command,{Seq,Action,Flags},State),
	Store = erlmail_conf:lookup_atom(store_type_mailbox_store,State),
	Selected = State#imapd_fsm.mailbox,
	{MailBoxName,UserName,DomainName} = Selected#mailbox_store.name,
	MailBox = Store:select({MailBoxName,{UserName,DomainName}}),
	Messages = imapd_util:seq_message_names(Seq,MailBox),
	_RespList = imapd_util:store(Messages,UserName,DomainName,Action,Flags),
%	?D(RespList),
	% @todo: Add responses for store status
	imapd_util:send(#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"},State),
	State#imapd_fsm{mailbox = MailBox};

%%%-------------------------
%%% COPY - Authenticated
%%%-------------------------

% @todo COPY impliment command

%%%-------------------------
%%% UID - Authenticated
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = uid = Command},
		#imapd_fsm{state = FSMState} = State) when FSMState =:= not_authenticated; FSMState =:= authenticated -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad},State),
	State;
command(#imap_cmd{tag = Tag, cmd = uid = Command, data = []}, State) -> 
	imapd_util:out(Command,State),
	imapd_util:send(#imap_resp{tag = Tag, status = bad, info = "Protocol Error: no data"},State),
	State;
command(#imap_cmd{tag = Tag, cmd = uid = Command, data = {fetch,Seq,Data}},#imapd_fsm{state = selected} = State) -> 
	Items = case lists:member(uid,Data) of
		true -> Data;
		flase -> lists:usort([uid|Data])
	end,
	imapd_util:out(Command,{fetch,Seq,Items},State),
	Selected = State#imapd_fsm.mailbox,
	{MailBoxName,UserName,DomainName} = Selected#mailbox_store.name,
	Store = erlmail_conf:lookup_atom(store_type_mailbox_store,State),
	MailBox = Store:select({MailBoxName,{UserName,DomainName}}),
	Messages = case Seq of
		all -> MailBox#mailbox_store.messages;
		Seq when is_list(Seq) -> [] % Check list membership for UID match in Seq
	end,
	RespList = imapd_fetch:fetch(Messages,Items,State),
	lists:map(fun(Resp) ->
		imapd_util:send(Resp,State)
		end, RespList),
	imapd_util:send(#imap_resp{tag = Tag, status = ok, cmd = Command, info = "Completed"},State),
	State#imapd_fsm{mailbox=MailBox};









%%%-------------------------
%%% CHECK EXTENTIONS OR CATCH ALL ERROR
%%%-------------------------
command(#imap_cmd{tag = Tag, cmd = Command, data = Data} = Cmd, State) ->
	case imapd_ext:check(Command) of
		{ok,Module,Function} ->  Module:Function(Cmd, State);
		{error,_Reason} ->
			imapd_util:out(Command,Data,State),
			imapd_util:send(#imap_resp{tag = Tag, status = bad},State),
			State
	end.