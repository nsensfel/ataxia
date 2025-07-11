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

-type message() :: holder() | category() | {'temp', category()}.

-record
(
	request,
	{
		is_write :: boolean(),
		client :: holder(),
		local_pid :: pid()
	}
).

-type request() :: #request{}.

-record
(
	lock,
	{
		db :: atom(),
		id :: ataxia_id:type(),
		status :: category(),
		timeout :: pid(),
		queue :: queue:queue(request()),
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
		category/0,
		message/0,
		holder/0
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
		create_holder/1,
		request/2,
		request_lock/4,
		request_lock/5,
		release_lock/2,
		release_lock/3,
		has_lock/3,
		new/2,
		timeout_process/1
	]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec grant (request(), category(), type()) -> type().
grant (Request, Category, State) ->
	Request#request.local_pid ! {ataxia_reply, lock, granted},
	State#lock
	{
		status = Category,
		holders = sets:add_element(Request#request.client, State#lock.holders)
	}.

-spec notify_all_reads (type()) -> type().
notify_all_reads (State) ->
	case queue:out(State#lock.queue) of
		{empty, _} -> State;
		{{value, Value}, NewQueue} ->
			case Value#request.is_write of
				true -> State;
				_ ->
					S0State = State#lock{ queue = NewQueue },
					S1State = grant(Value, read, S0State),
					notify_all_reads(S1State)
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
		{empty, _} -> S0State#lock{ status = unlocked };
		{{value, Value}, NewQueue} ->
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
					S1State = notify_all_reads(S0State),
					request_timeout(S1State),
					S1State#lock{ status = read }
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
init ([DB, ID]) ->
	{
		ok,
		#lock
		{
			db = DB,
			id = ID,
			status = unlocked,
			timeout = spawn(?MODULE, timeout_process, [self()]),
			queue = queue:new(),
			holders = sets:new()
		}
	}.

handle_call ({has_lock, Mode, Holder}, _From, State) ->
	Result =
		case {Mode, State#lock.status} of
			{A, B} when A == B -> sets:is_element(Holder, State#lock.holders);
			{read, write} -> sets:is_element(Holder, State#lock.holders);
			_ -> false
		end,
	{reply, Result, State}.

handle_cast (timeout, State) ->
	S0State = timeout(State),
	case S0State#lock.status of
		unlocked ->
			S0State#lock.timeout ! stop,
			{stop, {shutdown, {S0State#lock.db, S0State#lock.id}}, S0State};

		_ -> {noreply, S0State}
	end;
handle_cast ({write, LocalPID, Client}, State) ->
	Request =
		#request
		{
			client = Client,
			local_pid = LocalPID,
			is_write = true
		},
	case State#lock.status of
		unlocked -> {noreply, grant(Request, write, State)};
		_ ->
			Request =
				#request
				{
					client = Client,
					local_pid = LocalPID,
					is_write = true
				},
			NewState = State#lock{ queue = queue:in(Request, State#lock.queue) },
			{noreply, NewState}
	end;
handle_cast ({read, LocalPID, Client}, State) ->
	Request =
		#request
		{
			client = Client,
			local_pid = LocalPID,
			is_write = false
		},
	case State#lock.status of
		write ->
			NewState = State#lock{ queue = queue:in(Request, State#lock.queue) },
			{noreply, NewState};

		_ ->
			case (queue:len(State#lock.queue) == 0) of
				true ->
					S0State = grant(Request, read, State),
					request_timeout(S0State),
					{noreply, S0State};

				_ ->
					NewState =
						State#lock{ queue = queue:in(Request, State#lock.queue) },
					{noreply, NewState}
			end
	end;
handle_cast ({unlocked, Client}, State) ->
	S0State =
		State#lock{ holders = sets:del_element(Client, State#lock.holders) },

	S1State =
		case sets:is_empty(S0State#lock.holders) of
			true -> timeout(S0State);
			_ ->S0State
		end,

	case S1State#lock.status of
		unlocked ->
			S1State#lock.timeout ! stop,
			{stop, {shutdown, {S1State#lock.db, S1State#lock.id}}, S1State};

		_ -> {noreply, S1State}
	end.

terminate (Reason, FinalState) ->
	erlang:display({"Lock terminates.", Reason, FinalState}),
	{{Reason, FinalState}, FinalState}.

code_change (_, State, _) ->
	{ok, State}.

format_status (_, [_, State]) ->
	[{data, [{"State", State}]}].

handle_info(_, State) ->
	{noreply, State}.

new (DB, ID) ->
	{ok, Result} = gen_server:start_link(?MODULE, [DB, ID], []),
	Result.

-spec has_lock (pid(), holder(), category()) -> boolean().
has_lock (LockPID, Client, Mode) ->
	gen_server:call(LockPID, {has_lock, Mode, Client}).

-spec request (pid(), any()) -> 'ok'.
request (LockPID, Request) ->
	gen_server:cast(LockPID, Request),
	ok.

-spec request_lock (atom(), ataxia_id:type(), holder(), category()) -> holder().
request_lock (DB, ID, Client, Mode) ->
	LockPID = ataxia_lock_manager:request_lock_for(DB, ID, none),
	gen_server:cast(LockPID, {Mode, self(), Client}),
	receive
		{ataxia_reply, lock, granted} -> #holder{ node = node(), pid = LockPID }
	end.

-spec request_lock
	(
		atom(),
		ataxia_id:type(),
		node(),
		pid(),
		category()
	)
	-> holder().
request_lock (DB, ID, ClientNode, ClientPID, Mode) ->
	request_lock(DB, ID, #holder{ node = ClientNode, pid = ClientPID }, Mode).

-spec release_lock (pid(), holder()) -> 'ok'.
release_lock (LockPID, Client) ->
	gen_server:cast(LockPID, {unlocked, Client}),
	ok.

-spec release_lock (pid(), node(), pid()) -> 'ok'.
release_lock (LockPID, Node, PID) ->
	release_lock(LockPID, #holder{ node = Node, pid = PID }).

-spec create_holder (pid()) -> #holder{}.
create_holder (PID) -> #holder { node = node(), pid = PID }.
