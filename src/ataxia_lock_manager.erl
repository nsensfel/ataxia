%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ATAXIA LOCK MANAGER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ataxia_lock_manager).
-behavior(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type state() :: #{{atom(), ataxia_id:type()} := pid()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 'gen_server' Exports
-export
(
	[
		init/1,
		handle_cast/2,
		handle_call/3,
		terminate/2,
		code_change/3,
		format_status/2,
		handle_info/2
	]
).

-export
(
	[
		request_lock_for/3,
		start/0
	]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec request_lock (atom(), ataxia_id:type(), state()) ->
	{
		pid(),
		state()
	}.
request_lock (DB, ID, State) ->
	case maps:find({DB, ID}, State) of
		{ok, Result} -> {Result, State};
		error ->
			Result = ataxia_lock:new(DB, ID),
			NewState = maps:put({DB, ID}, Result, State),
			{Result, NewState}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 'gen_server' functions
init (_) ->
	process_flag(trap_exit, true),
	{ok, maps:new()}.

handle_call ({request_lock, DB, ID, none}, _From, State) ->
	{PID, NewState} = request_lock(DB, ID, State),
	{reply, PID, NewState};
handle_call ({request_lock, DB, ID, CurrentPID}, _From, State) ->
	{PID, NewState} =
		case maps:find({DB, ID}, State) of
			{ok, OtherValue} when OtherValue /= CurrentPID -> {OtherValue, State};
			_ -> request_lock(DB, ID, maps:remove({DB, ID}, State))
		end,

	{reply, PID, NewState}.

handle_cast (_Msg, State) -> {noreply, State}.

terminate (Reason, State) ->
	erlang:display({"Lock Manager terminated:", Reason, State}),
	ok.

code_change (_, State, _) ->
	{ok, State}.

format_status (_, [_, State]) ->
	[{data, [{"State", State}]}].

handle_info({'EXIT', _From, Reason}, State) ->
	case Reason of
		{shutdown, {DB, ID}} ->
			erlang:display({"Lock Manager removing lock", DB, ID}),
			{noreply, maps:remove({DB, ID}, State)};

		_ -> {noreply, State}
	end;
handle_info(What, State) ->
	erlang:display({"Lock Manager got other msg:", What, State}),
	{noreply, State}.


%%%% Interface Functions
-spec start () -> 'ok'.
start () ->
	gen_server:start({local, ataxia_lock_manager}, ?MODULE, [], []),
	ok.

-spec request_lock_for
	(
		atom(),
		ataxia_id:type(),
		(pid() | 'none')
	)
	-> pid().
request_lock_for (DB, ID, CurrentPID) ->
	gen_server:call
	(
		ataxia_lock_manager,
		{request_lock, DB, ID, CurrentPID}
	).
