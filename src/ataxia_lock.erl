-module(ataxia_lock).
-behavior(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type category() :: unlocked | read_lock | write_lock.

-record
(
	lock_stored_request,
	{
		is_write :: bool(),
		node :: node(),
		pid :: pid()
	}
).

-record
(
	request,
	{
		db :: atom(),
		id :: ataxia_id:type(),
		category :: category()
	}
).

-record
(
	lock,
	{
		db :: atom(),
		id :: ataxia_id:type(),
		status :: category(),
		timeout :: pid(),
		queue :: queue(lock_stored_request()),
		holders :: [lock_stored_request()]
	}
).

-type type() :: #lock{}.
-type stored_request() :: #lock_stored_request{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type
(
	[
		type/0,
		request/0,
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
		handle_requests/2,
		write_lock_stored_request/2,
		read_lock_stored_request/2,
		release_request/2,
		start/0
	]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec grant (lock_stored_request(), category(), type()) -> type().
grant (Request, Msg, State) ->
	{Request#lock_stored_request.node, Request#lock_stored_request.pid} !
		{granted,
			#request
			{
				id = State#lock.id,
				db = State#lock.db,
				category = Msg
			}
		},
	State#lock
	{
		holders = ordsets:add_element(Request, State#lock.holders)
	}.

-spec notify_all_read_locks (type()) -> type().
notify_all_read_locks (State) ->
	case queue:out(State#lock.queue) of
		empty -> State;
		{{ok, Value}, NewQueue} ->
			if Value#lock_stored_request.is_write
				State
			else
				S0State#lock{ queue = NewQueue },
				S1State = grant(Value, read_lock, S0State),
				notify_all_read_locks(B, S1State)
			end
	end

-spec request_timeout (type()) -> ok.
request_timeout (State) ->
	State#lock.timeout ! ping,
	ok.

-spec timeout (type()) -> type().
timeout (State) ->
	S0State = State#lock{ holders = ordsets:new() },
	case queue:out(S0State#lock.queue) of
		empty -> S0State#lock{ status = unlocked };
		{{ok, Value}, NewQueue} ->
			if Value#lock_stored_request.is_write
				S1State = grant(Value, write_lock, S0State),
				request_timeout(S1State),
				S1State#lock
				{
					queue = NewQueue,
					status = write_lock
				}
			else
				S1State = notify_all_readlocks(S0State),
				request_timeout(S1State),
				S1State#lock{ status = read_locked }
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
init ({DB, ID}) ->
	{
		ok,
		#lock
		{
			db = DB,
			id = ID,
			status = unlocked,
			timeout = spawn(?MODULE, timeout_process, [pid()]),
			queue = queue:new(),
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
handle_cast ({write_lock, Node, PID}, State) ->
	Request = #lock_stored_request{ node = Node, pid = PID, is_write = true },
	NewState = State#lock{ queue = queue:in(Request, State#lock.queue) }
	{noreply, NewState};
handle_cast ({read_lock, Node, PID}, State) ->
	Request = #lock_stored_request{ node = Node, pid = PID, is_write = false },
	case State#lock.status of
		write_lock ->
			NewState = State#lock{ queue = queue:in(Request, State#lock.queue) },
			{noreply, NewState};

		_ ->
			S0State = grant(Request, read_lock, State),
			request_timeout(S0State),
			{noreply, S0State}
	end;
handle_cast ({unlocked, IsWrite, Node, PID}, State) ->
	Request = #lock_stored_request{ node = Node, pid = PID, is_write = IsWrite },
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

handle_info(_, State) ->
	{noreply, State}.

%%%% Interface Functions
% handle_requests: [request], {node(), pid()}. To sort the requests by ID
% so that if two processes want the 2 same locks, they're much less
% likely to both acquire one and block the other process.
-spec handle_requests ([request()]) -> pid().
handle_requests (Requests) ->

		create_request/3,
		request_write_lock/2,
		request_read_lock/2,
		request_lock_release/2,
		start/0
