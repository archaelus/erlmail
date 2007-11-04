%%%----------------------------------------------------------------------
%%% File        : imapd_fsm
%%% Author      : Stuart Jackson <sjackson@simpleenigma.com> [http://www.simpleenigma.com]
%%% Purpose     : IMAP Client API - Use these funcations instead of the FSM
%%% Created     : 2007-10-21
%%% Initial Rel : 0.0.6
%%% Updated     : 2007-10-21
%%%----------------------------------------------------------------------

-module(imapd_fsm).
-author('sjackson@simpleenigma.com').
-include("../include/imap.hrl").

-behaviour(gen_fsm).

-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
    'WAIT_FOR_SOCKET'/2,
    'WAIT_FOR_DATA'/2
]).

-define(TIMEOUT, 10 * 60 * 10000).

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------


start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
	Options = erlmail_conf:read(),
    {ok, 'WAIT_FOR_SOCKET', #imapd_fsm{options = Options}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
    % Now we own the socket
    inet:setopts(Socket, [{active, once}, binary]),
    {ok, {IP, _Port}} = inet:peername(Socket),
	NextState = State#imapd_fsm{socket=Socket, addr=IP},
	Greeting = imapd_util:greeting(State),
	case imapd_util:greeting_capability(State) of
		true ->
			Capability = "CAPABILITY " ++ imapd_ext:capability(),
			imapd_util:out(greeting,Capability,NextState),
			imapd_util:send(#imap_resp{tag = '*', status = ok, code = {capability,Capability}, info = Greeting},Socket);
		false ->
			imapd_util:out(greeting,NextState),
			imapd_util:send(#imap_resp{tag = '*', status = ok, info = Greeting},Socket)
		
	end,
    {next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};
'WAIT_FOR_SOCKET'(Other, State) ->
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, #imapd_fsm{buff = Buff} = State) ->
	NewBuff = <<Buff/binary,Data/binary>>,
%	?D(NewBuff),
	case end_of_cmd(NewBuff) of
		0 -> {next_state, 'WAIT_FOR_DATA', State#imapd_fsm{buff = NewBuff}, ?TIMEOUT};
		Pos ->
			<<Line:Pos/binary,13,10,NextBuff/binary>> = NewBuff,
			NextState = imapd_cmd:command(State#imapd_fsm{line = binary_to_list(Line),buff = NextBuff}),

			{next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT}
	end;
    

'WAIT_FOR_DATA'(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
    {stop, normal, State};

'WAIT_FOR_DATA'(Data, State) ->
    io:format("~p Ignoring data: ~p\n", [self(), Data]),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, #imapd_fsm{socket=Socket} = StateData) ->
    % Flow control: enable forwarding of next TCP message
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName,
            #imapd_fsm{socket=Socket, addr=Addr} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #imapd_fsm{socket=Socket}) ->
    (catch gen_tcp:close(Socket)),
    ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

















end_of_cmd(Bin) ->
	List = binary_to_list(Bin),
	case string:str(List,?CRLF) of
		0 -> 0;
		Pos -> Pos - 1
	end.