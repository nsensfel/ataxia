-module(ataxia_lock).
-behavior(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type status() :: unlocked | read_locked | write_locked
-record
(
	lock_request,
	{
		is_write :: bool(),
		node :: node(),
		pid :: pid()
	}
).

-record
(
	lock,
	{
		status :: status(),
		timeout :: pid(),
		queue :: [lock_request()],
		holders :: [lock_request()]
	}
).

-type type() :: #lock{}.

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
		write_lock_request/2,
		read_lock_request/1,
		start/0
	]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec grant (lock_request(), atom(), type()) -> type().
grant (Request, Msg, Lock) ->
	{Request#lock_request.node, Request#lock_request.pid} ! Msg,
	Status#lock
	{
		holders = ordsets:add_element(Request, Status#lock.holders)
	}.

-spec notify_all_read_locks (list(lock_request()), type()) -> type().
notify_all_read_locks ([], State) -> State#lock { queue = [] };
notify_all_read_locks ([A | B], S0State) when !A#lock_request.is_write ->
	S1State = grant(A, read_lock, S0State),
	notify_all_read_locks (B, S1State);
notify_all_read_locks (RemainingQueue, State) ->
	State#lock { queue = RemainingQueue }.

-spec request_timeout (type()) -> ok.
request_timeout (State) ->
	State#lock.timeout ! ping,
	ok.

-spec timeout (type()) -> type().
timeout (S0State) ->
	State = S0State#lock{ holders = ordsets:new() },
	case State#lock.queue of
		[] -> State#lock{ status = unlocked };
		[A | B] when A#lock_request.is_write ->
			S1State = grant(A, write_lock, State),
			request_timeout(S1State),
			S1State#lock
			{
				queue = B,
				status = write_locked
			};
		Other ->
			S1State = notify_all_readlocks(Other),
			request_timeout(S1State),
			S1State#lock { status = read_locked }
	end.

-spec timeout_process (pid()) -> ok.
timeout_process (Pid) ->
	receive
		ping -> timeout_process(Pid);
		quit -> ok
	after 10000 ->
		Pid ! timeout,
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
			queue = [],
			holders = ordsets:new()
		}
	};

handle_cast (timeout, State) ->
	S0State = timeout(State),
	case S0tate#lock.status of
		unlocked ->
			S0State#lock.timeout ! stop,
			{stop, timeout, State};

		_ -> {noreply, S0State}
	end;
handle_cast ({write_lock_request, Node, PID}, State) ->
	Request = #lock_request{ node = Node, pid = PID, is_write = true },
	NewState = State#lock{ queue = State#lock.queue ++ [Request] },
	{noreply, NewState};
handle_cast ({read_lock_request, Node, PID}, State) ->
	Request = #lock_request{ node = Node, pid = PID, is_write = false },
	case State#lock.status of
		write_locked ->
			NewState = State#lock{ queue = State#lock.queue ++ [Request] },
			{noreply, NewState};

		_ ->
			S0State = grant(Request, read_lock, State),
			request_timeout(S0State),
			{noreply, S0State}
	end;
handle_cast ({lock_release, IsWrite, Node, PID}, State) ->
	Request = #lock_request{ node = Node, pid = PID, is_write = IsWrite},
	S0State =
		State#lock
		{
			holders = ordsets:del_element(Request, State#lock.holders)
		},

	S1State =
		if ordsets:is_empty(S0State#lock.holders)
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

handle_info(timeout, State) ->
	case State#lock.status of
		unlocked ->
			State#lock.timeout ! stop,
			{stop, timeout, State}.
		_ -> {noreply, State, State#lock.timeout * 1000};
handle_info(_, State) ->
	{noreply, State, ataxia_time:seconds_before(State#lock.timeout) * 1000}.

%%%% Interface Functions
