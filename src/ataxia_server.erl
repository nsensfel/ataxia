-module(ataxia_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
	[
		fetch/5,
		blind_update/4,
		safe_update/6,
		blind_update_then_fetch/4,
		blind_remove/3,
		safe_remove/4,
		fetch_if/4,
		blind_update_if/5,
		blind_update_if_then_fetch/5,
		blind_update_if_else_fetch/5,
		blind_remove_if/4
	]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec await_lock_reply (atom(), ataxia_id:type(), any(), pid()) -> any().
await_lock_reply (DB, ID, Request, LockPID) ->
	receive
		{ataxia_reply, lock, Reply} -> Reply;
	after ?TIMEOUT ->
		request_new_lock_handler_of(DB, ID, Request, LockPID)
	end.

-spec request_new_lock_handler_of
(
	atom(),
	ataxia_id:type(),
	any(),
	(pid() | none)
)
-> any().
request_new_lock_handler_of (Client, DB, ID, Request, LockPID) ->
	NewLockPID = ataxia_lock_manager:request_lock_for(DB, ID, LockPID),
	ataxia_lock:request(NewLockPID, Request),
	case is_process_alive(NewLockPID) of
		true -> await_lock_reply(DB, ID, Request, NewLockPID);
		_ -> request_new_lock_handler_of(DB, ID, Request, NewLockPID)
	end.

-spec request_lock (atom(), ataxia_id:type(), any()) -> any().
request_lock (DB, ID, Request) ->
	request_new_lock_handler_of(DB, ID, Request, none).

-spec handle_lock_message (ataxia_lock:message()) -> ???
handle_lock_message (_DB, _ID, unlocked) -> {'error', 'lock'};
handle_lock_message (DB, ID, read) ->
	LockPid = request_lock(DB, ID, ),
		case get_lock_pid(ProcessedLock) of
			error -> {error, lock};
			LockPID ->
				case ataxia_lock:has_lock(LockPID, Client, read, LockPID) of
					false -> {error, lock};
					true ->
						case ataxia_database:read(DB, ID) of
							error -> {error, id};
							Entry ->
								case {Version, ataxia_entry:get_version(Entry)} of
									{A, B} if A == B -> ok;
									{_, NewVersion} ->
										{ok, NewVersion, ataxia_entry:get_value(Entry)}
								end
						end
				end
		end,

act_with_lock ({fetch, DB, ID, Version}) ->
	case ataxia_database:read(DB, ID) of
		error -> {error, id};
		{ok, Entry} ->
			CurrentVersion = ataxia_entry:get_version(Entry),
			case CurrentVersion == Version of
				true -> {ok, ok};
				_ -> {ok, {CurrentVersion, ataxia_entry:get_value(Entry)}}
			end
	end;
act_with_lock ({blind_update, DB, ID, Op}) ->
	case ataxia_database:read(DB, ID) of
		error -> {error, id};
		{ok, S0Entry} ->
			S1Entry = ataxia_entry:increase_version(S0Entry),
			S2Entry =
				ataxia_entry:set_value
				(
					ataxic:apply_to(Op, ataxia_entry:get_value(S1Entry)),
					S1Entry
				),
			ataxia_database:write(DB, ID, S2Entry),
			{ok, ataxia_entry:get_version(S2Entry)}
	end;
act_with_lock ({safe_update, DB, ID, ExpectedVersion, Op}) ->
	case ataxia_database:read(DB, ID) of
		error -> {error, id};
		{ok, S0Entry} ->
			case ExpectedVersion == ataxia_entry:get_version(S0Entry) of
				true ->
					S1Entry = ataxia_entry:increase_version(S0Entry),
					S2Entry =
						ataxia_entry:set_value
						(
							ataxic:apply_to(Op, ataxia_entry:get_value(S1Entry)),
							S1Entry
						),
					ataxia_database:write(DB, ID, S2Entry),
					{ok, ataxia_entry:get_version(S2Entry)}

				_ -> {error, version}
			end
	end;
act_with_lock ({blind_update_then_fetch, DB, ID, Op}) ->
	case ataxia_database:read(DB, ID) of
		error -> {error, id};
		{ok, S0Entry} ->
			S1Entry = ataxia_entry:increase_version(S0Entry),
			S2Entry =
				ataxia_entry:set_value
				(
					ataxic:apply_to(Op, ataxia_entry:get_value(S1Entry)),
					S1Entry
				),
			ataxia_database:write(DB, ID, S2Entry),
			{
				ok,
				ataxia_entry:get_version(S2Entry),
				ataxia_entry:get_value(S2Entry)
			}
	end;
act_with_lock ({blind_remove, DB, ID}) ->
	case ataxia_database:delete(DB, ID) of
		error -> {error, id};
		ok -> {ok, ok}
	end;
act_with_lock ({safe_remove, DB, ID, ExpectedVersion}) ->
	case ataxia_database:read(DB, ID) of
		error -> {error, id};
		{ok, S0Entry} ->
			case ExpectedVersion == ataxia_entry:get_version(S0Entry) of
				true ->
					case ataxia_database:delete(DB, ID) of
						error -> {error, id};
						ok -> {ok, ok}
					end;

				_ -> {error, version}
			end
	end;
act_with_lock ({fetch_if, DB, ID, Version, Cond}) ->
	case ataxia_database:read(DB, ID) of
		error -> {error, id};
		{ok, Entry} ->
			CurrentVersion = ataxia_entry:get_version(Entry),
			case
				{
					ataxic:apply_to(Cond, ataxia_entry:get_value(Entry)),
					CurrentVersion == Version
				}
			of
				{true, true} -> {ok, ok};
				{true, false} ->
					{ok, {CurrentVersion, ataxia_entry:get_value(Entry)}};

				_ -> {error, condition}
			end
	end;
act_with_lock ({blind_update_if, DB, ID, Cond, Op}) ->
	case ataxia_database:read(DB, ID) of
		error -> {error, id};
		{ok, S0Entry} ->
			case ataxic:apply_to(Cond, ataxia_entry:get_value(S0Entry)) of
				true ->
					S1Entry = ataxia_entry:increase_version(S0Entry),
					S2Entry =
						ataxia_entry:set_value
						(
							ataxic:apply_to(Op, ataxia_entry:get_value(S1Entry)),
							S1Entry
						),
					ataxia_database:write(DB, ID, S2Entry),
					{ok, ataxia_entry:get_version(S2Entry)};

				_ -> {error, condition}
			end
	end;
act_with_lock ({blind_update_if_then_fetch, DB, ID, Cond, Op}) ->
	case ataxia_database:read(DB, ID) of
		error -> {error, id};
		{ok, S0Entry} ->
			case ataxic:apply_to(Cond, ataxia_entry:get_value(S0Entry)) of
				true ->
					S1Entry = ataxia_entry:increase_version(S0Entry),
					S2Entry =
						ataxia_entry:set_value
						(
							ataxic:apply_to(Op, ataxia_entry:get_value(S1Entry)),
							S1Entry
						),
					ataxia_database:write(DB, ID, S2Entry),
					{
						ok,
						{
							ataxia_entry:get_version(S2Entry),
							ataxia_entry:get_value(S2Entry)
						}
					};

				_ -> {error, condition}
			end
	end;
act_with_lock ({blind_update_if_else_fetch, DB, ID, Cond, Op}) ->
	case ataxia_database:read(DB, ID) of
		error -> {error, id};
		{ok, S0Entry} ->
			case ataxic:apply_to(Cond, ataxia_entry:get_value(S0Entry)) of
				true ->
					S1Entry = ataxia_entry:increase_version(S0Entry),
					S2Entry =
						ataxia_entry:set_value
						(
							ataxic:apply_to(Op, ataxia_entry:get_value(S1Entry)),
							S1Entry
						),
					ataxia_database:write(DB, ID, S2Entry),
					{ok, {ok, updated, ataxia_entry:get_version(S2Entry)}};

				_ ->
					{
						ok,
						{
							ok,
							fetched,
							ataxia_entry:get_version(S0Entry),
							ataxia_entry:get_value(S0Entry)
						}
					}
			end
	end;
act_with_lock ({blind_remove_if, DB, ID, Cond}) ->
	case ataxia_database:read(DB, ID) of
		error -> {error, id};
		{ok, Entry} ->
			case ataxic:apply_to(Cond, ataxia_entry:get_value(Entry)) of
				true ->
					case ataxia_database:delete(DB, ID) of
						error -> {error, id};
						ok -> {ok, ok}
					end;

				_ -> {error, condition}
			end
	end.

-spec perform_with_lock
(
	ataxia_lock:holder(),
	atom(),
	ataxia_id:type(),
	ataxia_lock:message(),
	ataxia_lock:category(),
	any()
) -> any().
perform_with_lock (_Client, _DB, _ID, unlocked, _Perm, _Req) -> {error, lock};
perform_with_lock (_C, _DB, _ID, {temp, unlocked}, _P, _R) -> {error, lock};
perform_with_lock (_Client, _DB, _ID, read, write, _Req) -> {error, lock};
perform_with_lock (_C, _DB, _ID, {temp, read}, write, _Req) -> {error, lock};
perform_with_lock (Client, DB, ID, read, read, Request) ->
	LockRequest = {read, self(), Client},
	{granted, Lock} = request_lock(DB, ID, LockRequest),
	case act_with_lock(Request) of
		{ok, Data} -> {ok, Lock, Data};
		{error, Error} ->
			ataxia_lock:release_lock(Lock, Client),
			{error, Error}
	end;
perform_with_lock (Client, DB, ID, write, _Permission, Request) ->
	LockRequest = {write, self(), Client},
	{granted, Lock} = request_lock(DB, ID, LockRequest),
	case act_with_lock(Request) of
		{ok, Data} -> {ok, Lock, Data};
		{error, Error} ->
			ataxia_lock:release_lock(Lock, Client),
			{error, Error}
	end;
perform_with_lock (Client, DB, ID, {temp, read}, read, Request) ->
	LockRequest = {read, self(), Client},
	{granted, Lock} = request_lock(DB, ID, LockRequest),
	Output =
		case act_with_lock(Request) of
			{ok, Data} -> {ok, Data};
			{error, Error} -> {error, Error}
		end,
	ataxia_lock:release_lock(Lock, Client),
	Output;
perform_with_lock (Client, DB, ID, write, _Permission, Request) ->
	LockRequest = {write, self(), Client},
	{granted, Lock} = request_lock(DB, ID, LockRequest),
	Output =
		case act_with_lock(Request) of
			{ok, Data} -> {ok, Lock, Data};
			{error, Error} -> {error, Error}
		end,
	ataxia_lock:release_lock(Lock, Client),
	Output;
perform_with_lock (Client, _DB, _ID, LockRef, Permission, Request) ->
	case ataxia_lock:has_lock(LockRef, Client, Permission) of
		true -> act_with_lock(Request);
		_ -> {error, lock}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec fetch
	(
		ataxia_lock:holder(),
		atom(),
		ataxia_id:type(),
		ataxia_lock:message(),
		(non_neg_integer() | 'none')
	)
	->
	(
		'ok'
		| {'ok', ataxia_lock:message()}
		| {'ok', non_neg_integer(), any()}
		| {'ok', ataxia_lock:message(), non_neg_integer(), any()}
		| ataxia_error:type()
	).
fetch (Client, DB, ID, Lock, Version) ->
	Request = {fetch, DB, ID, Version},
	Permission = read,
	perform_with_lock(Client, DB, ID, Lock, Permission, Request).

-spec blind_update
	(
		ataxia_lock:holder(),
		atom(),
		ataxia_id:type(),
		ataxia_lock:message(),
		ataxic:type()
	)
	->
	(
		{'ok', ataxia_lock:holder(), non_neg_integer()}
		| {'ok', non_neg_integer()}
		| ataxia_error:type()
	).
blind_update (Client, DB, ID, Lock, Op) ->
	Request = {blind_update, DB, ID, Op},
	Permission = write,
	perform_with_lock(Client, DB, ID, Lock, Permission, Request).

		safe_update/6,
		blind_update_then_fetch/4,
		blind_remove/3,
		safe_remove/4,
		fetch_if/4,
		blind_update_if/5,
		blind_update_if_then_fetch/5,
		blind_update_if_else_fetch/5,
		blind_remove_if/4

