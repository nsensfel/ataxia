-module(ataxia_lock).
-behavior(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type category() :: unlocked | read | write.

-record
(
	holder,
	{
		node :: node(),
		pid :: pid()
	}
).

-type holder() :: #holder{}.

-record
(
	request,
	{
		is_write :: bool(),
		client :: holder(),
		local_pid :: pid()
	}
).

-type request() :: #request{}.

-record
(
	lock,
	{
		status :: category(),
		timeout :: pid(),
		queue :: queue(request()),
		holders :: sets:set(holder())
	}
).

-type type() :: #lock{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type
(
	[
		type/0,
		category/0
	]
).

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
		request_lock/2,
		write_stored_request/2,
		read_stored_request/2,
		release_request/2,
		start/0
	]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec grant (request(), category(), type()) -> type().
grant (Request, Category, State) ->
	Request#request.local_pid ! granted,
	State#lock
	{
		status = Category,
		holders = sets:add_element(Request#client, State#lock.holders)
	}.

-spec notify_all_reads (type()) -> type().
notify_all_reads (State) ->
	case queue:out(State#lock.queue) of
		empty -> State;
		{{ok, Value}, NewQueue} ->
			case Value#request.is_write of
				true -> State;
				_ ->
					S0State#lock{ queue = NewQueue },
					S1State = grant(Value, read, S0State),
					notify_all_reads(B, S1State)
			end
	end.

-spec request_timeout (type()) -> ok.
request_timeout (State) ->
	State#lock.timeout ! ping,
	ok.

-spec timeout (type()) -> type().
timeout (State) ->
	S0State = State#lock{ holders = sets:new() },
	case queue:out(S0State#lock.queue) of
		empty -> S0State#lock{ status = unlocked };
		{{ok, Value}, NewQueue} ->
			case Value#request.is_write of
				true ->
					S1State = grant(Value, write, S0State),
					request_timeout(S1State),
					S1State#lock
					{
						queue = NewQueue,
						status = write
					};

				_ ->
					S1State = notify_all_readlocks(S0State),
					request_timeout(S1State),
					S1State#lock{ status = readed }
			end
	end.

-spec timeout_process (pid()) -> ok.
timeout_process (Pid) ->
	receive
		ping -> timeout_process(Pid);
		quit -> ok
	after 10000 ->
		gen_server:cast(Pid, timeout),
		timeout_process(Pid)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 'gen_server' functions
init (_) ->
	{
		ok,
		#lock
		{
			status = unlocked,
			timeout = spawn(?MODULE, timeout_process, [pid()]),
			queue = queue:new(),
			holders = sets:new()
		}
	};

handle_call ({has_lock, Mode, Holder}, State) ->
	Result =
		case {Mode, State#lock.status} of
			{A, B} when A == B -> sets:is_element(Holder, State#lock.holders);
			{read, write} -> sets:is_element(Holder, State#lock.holders);
			_ -> false
		end,
	{reply, Result, State}.

handle_cast (timeout, State) ->
	S0State = timeout(State),
	case S0tate#lock.status of
		unlocked ->
			S0State#lock.timeout ! stop,
			{stop, timeout, State};

		_ -> {noreply, S0State}
	end;
handle_cast ({write, LocalPID, ClientNode, ClientPID}, State) ->
	case State#lock.status of
		unlocked -> {noreply, grant(Request, write, State)};
		_ ->
			Request =
				#request
				{
					client = #holder{ pid = ClientPID, node = ClientNode },
					local_pid = LocalPID,
					is_write = true
				},
			NewState = State#lock{ queue = queue:in(Request, State#lock.queue) }
			{noreply, NewState}
	end;
handle_cast ({read, LocalPID, ClientNode, ClientPID}, State) ->
	Request =
		#request
		{
			client = #holder{ pid = ClientPID, node = ClientNode },
			local_pid = LocalPID,
			is_write = false
		},
	case State#lock.status of
		write ->
			NewState = State#lock{ queue = queue:in(Request, State#lock.queue) },
			{noreply, NewState};

		_ when (queue:len(State#queue) == 0) ->
			S0State = grant(Request, read, State),
			request_timeout(S0State),
			{noreply, S0State};

		_ ->
			NewState = State#lock{ queue = queue:in(Request, State#lock.queue) },
			{noreply, NewState};
	end;
handle_cast ({unlocked, ClientNode, ClientPID}, State) ->
	Holder = #holder{ node = ClientNode, pid = ClientPID }),
	S0State =
		State#lock{ holders = sets:del_element(Holder, State#lock.holders) },

	S1State =
		if sets:is_empty(S0State#lock.holders)
			timeout(S0State)
		else
			S0State
		end,

	case S0tate#lock.status of
		unlocked ->
			S0State#lock.timeout ! stop,
			{stop, timeout, State};

		_ -> {noreply, S0State}
	end.

terminate (_, _) -> ok.

code_change (_, State, _) ->
	{ok, State}.

format_status (_, [_, State]) ->
	[{data, [{"State", State}]}].

handle_info(_, State) ->
	{noreply, State}.

new () ->
	gen_server:start_link(?MODULE, [], []).

has_lock (ClientNode, ClientPID, Mode, LockPID) ->
	gen_server:call
	(
		LockPID,
		{has_lock, Mode, #holder{ node = ClientNode, pid = ClientPID } }
	).

request_lock (DB, ID, ClientNode, ClientPID, Mode) ->
	{ok, LockPID} = gen_server:call(ataxia_lock_manager, get_lock, [DB, ID]),
	gen_server:cast(LockPID, {Mode, self(), ClientNode, ClientPID}),
	receive
		granted -> {node(), LockPID}
	end.

release_lock (UserNode, UserPID, LockPid) ->
	gen_server:cast(LockPID, {unlocked, ClientNode, ClientPID}).
