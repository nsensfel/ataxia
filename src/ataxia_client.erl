-module(ataxia_client).
%%%% This should be the main client interface, handling cache and locks.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
	client,
	{
		known_cache_entries :: orddict({DB, ID}, pid()),
		cache_entry_managers :: orddict(non_neg_integer(), pid()),
		next_request_id :: non_neg_integer()
	}
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
	[
		new/0,
		merge/2, % merges 2 clients from 2 asynchronous calls.
	]
).

-export
(
	[
		add/5,
		add_at/6,
		reserve/4,
		reserve_at/5,

		fetch/3,
		update/4,
		update_and_fetch/4,
		remove/3,

		fetch_if/4,
		update_if/5,
		update_and_fetch_if/5,
		update_if_else_fetch/5,
		remove_if/4,

		% TODO any_where, all_where variants.
		fetch_any/3,
		update_any/4,
		update_and_fetch_any/4,
		remove_any/3,

		fetch_all/3,
		update_all/4,
		update_and_fetch_all/4,
		remove_all/3
	]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_debug_db_node () -> node().
get_debug_db_node () -> list_to_atom("db_node@" ++ net_adm:localhost()).

-spec get_random_db_node () -> node().
get_random_db_node () ->
	get_debug_db_node().

-spec get_db_node_for (ataxia_id:type()) -> node().
get_db_node_for (_ObjectID) ->
	get_debug_db_node().

-spec await_reply
(
	type(),
	atom(),
	ataxia_id:type(),
	any(),
	pid()
)
-> {type(), any()}.
await_reply (Client, DB, ID, Request, EntryPID, RequestID) ->
	receive
		{ataxia_reply, ReplyToID, Reply} when ReplyToID == RequestID ->
			{Client, Reply}
	after ?TIMEOUT ->
		request_new_handler_of(Client, DB, ID, Request, EntryPID)
	end.

-spec request_new_handler_of
(
	type(),
	atom(),
	ataxia_id:type(),
	any(),
	(pid() | none)
)
-> {type(), any()}.
request_new_handler_of (Client, DB, ID, Request, EntryPID) ->
	ManagerKey = ataxia_cache_manager:get_key_for(DB, ID),
	ManagerPID = orddict:fetch(ManagerKey),
	NewEntryPID =
		ataxia_cache_manager:request_entry_for
		(
			ManagerPID,
			DB,
			ID,
			EntryPID
		),
	ataxia_cache_entry:request(NewEntryPID, Request),
	case is_process_alive(NewEntryPID) of
		true ->
			await_reply
			(
				Client#client
				{
					known_cache_entries =
						orddict:store
						(
							{DB, ID},
							NewEntryPID,
							Client#client.known_cache_entries
						)
				},
				DB,
				ID,
				Request,
				NewEntryPID
			);

		false -> request_new_handler_of(Client, DB, ID, Request, NewEntryPID)
	end.

-spec request
(
	type(),
	atom(),
	ataxia_id:type(),
	any()
)
-> {type(), any()}.
request (Client, DB, ID, Request) ->
	case orddict:find(Client#client.known_cache_entries, {BD, ID}) of
		{ok, EntryPID} ->
			ataxia_cache_entry:request(NewEntryPID, Request),
			case is_process_alive(EntryPID) of
				true -> await_reply(Client, DB, ID, Request, EntryPID);
				false -> request_new_handler_of(Client, DB, ID, Request, EntryPID)
			end;

		error -> request_new_handler_of(Client, DB, ID, Request, none)
	end.

	CacheEntry = ataxia_cache_manager:request_cache_entry(Client, DB, ID),
	ataxia_cache_entry:request(Client, DB, ID, {blind_update, DB, ID}).

-spec merge_cache_entry_dicts
	(
		type(),
		atom(),
		ataxia_id:type(),
		(pid() | 'none'),
		(pid() | 'none')
	)
	-> (pid() | 'none').
merge_cache_entry_dicts (_Client, _DB, _ID, A, B) when A == B -> A;
merge_cache_entry_dicts (Client, DB, ID, _A, _B) ->
	ManagerKey = ataxia_cache_manager:get_key_for(DB, ID),
	ManagerPID = orddict:fetch(ManagerKey, Client#client.cache_entry_managers),
	ataxia_cache_manager:peek_entry_for(ManagerPID, DB, ID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new () -> type().
new () ->
	#client
	{
		known_cache_entries = orddict:new(),
		cache_entry_managers = ataxia_cache_manager:get_cache_entry_managers()
	}.

-spec merge (type(), type()) -> type().
merge (ClientA, ClientB) ->
	ClientA#client
	{
		known_cache_entries =
			orddict:filter
			(
				fun (_Key, Value) -> Value /= none end,
				orddict:merge
				(
					fun ({DB, ID}, A, B) ->
						merge_cache_entry_dicts(ClientA, DB, ID, A, B)
					end,
					ClientA#client.known_cache_entries,
					ClientB#client.known_cache_entries
				)
			)
	}.

%%%% ADD NEW ELEMENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% FIXME: we likely need a table containing the ID tracker of the other
%%%% tables.
%%%%
%%%% Providing everything
%%%%
-spec add_at (atom(), any(), ataxia_id:type()) -> ('ok' | ataxia_error:type()).
add_at (Client, DB, Value, ID) ->
	DBNode = get_db_node_for(ID),
	rpc:call(DBNode, ataxia_server, add_at, [DB, Value, ID]).

-spec add (atom(), any()) -> ({'ok', ataxia_id:type()} | ataxia_error:type()).
add (Client, DB, Value) ->
	DBNode = get_random_db_node(),
	rpc:call ( DBNode, ataxia_server, add, [DB, Value]).

-spec reserve (atom()) -> ({'ok', ataxia_id:type()} | ataxia_error:type()).
reserve (Client, DB, ReadPerm, WritePerm, Lock) ->
	DBNode = get_random_db_node(),
	rpc:call(DBNode, ataxia_server, reserve, [DB]).

-spec reserve_at (atom(), ataxia_id:type()) -> ('ok' | ataxia_error:type()).
reserve_at (Client, DB, ID) ->
	DBNode = get_db_node_for(ID),
	rpc:call(DBNode, ataxia_server, reserve_at, [DB, ID]).

%%%% BY ID %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% FIXME: these requests on the cache manager are a bottleneck.
-spec fetch
	(
		atom(),
		ataxia_id:type()
	)
	-> ({'ok', non_neg_integer(), any()} | ataxia_error:type()).
fetch (Client, DB, ID) ->
	CacheEntry = ataxia_cache_manager:request_cache_entry(Client, DB, ID),
	ataxia_cache_entry:request(Client, DB, ID, {fetch, DB, ID}).

-spec blind_update
	(
		atom(),
		ataxia_id:type(),
		ataxic:operation()
	)
	-> ('ok' | ataxia_error:type()).
blind_update (Client, DB, ID, Op) ->
	CacheEntry = ataxia_cache_manager:request_cache_entry(Client, DB, ID),
	ataxia_cache_entry:request(Client, DB, ID, {blind_update, DB, ID}).

-spec safe_update
	(
		atom(),
		ataxia_id:type(),
		ataxic:operation(),
		non_neg_integer(),
		any()
	)
	-> ({'ok', non_neg_integer()} | ataxia_error:type()).
safe_update (Client, DB, ID, Op, ExpectedCurrentVersion, ExpectedNewValue) ->
	CacheEntry = ataxia_cache_manager:request_cache_entry(Client, DB, ID),
	ataxia_cache_entry:request(Client, DB, ID, {safe_update, DB, ID, Op, ExpectedCurrentVersion, ExpectedNewValue}).

-spec blind_update_then_fetch
	(
		atom(),
		ataxia_id:type(),
		ataxic:operation()
	)
	-> ({'ok', non_neg_integer(), any()} | ataxia_error:type()).
blind_update_then_fetch (Client, DB, ID, OP) ->
	CacheEntry = ataxia_cache_manager:request_cache_entry(Client, DB, ID),
	ataxia_cache_entry:request(Client, DB, ID, {blind_update_then_fetch, DB, ID, Op}).

-spec blind_remove (atom(), ataxia_id:type()) -> ('ok' | ataxia_error:type()).
blind_remove (Client, DB, ID) ->
	CacheEntry = ataxia_cache_manager:request_cache_entry(Client, DB, ID),
	ataxia_cache_entry:request(Client, DB, ID, {blind_remove, DB, ID}).

-spec safe_remove
	(
		atom(),
		ataxia_id:type(),
		non_neg_integer()
	)
	-> ('ok' | ataxia_error:type()).
safe_remove (Client, DB, ID, ExpectedCurrentVersion) ->
	CacheEntry = ataxia_cache_manager:request_cache_entry(Client, DB, ID),
	ataxia_cache_entry:request(Client, DB, ID, {safe_remove, DB, ID, ExpectedCurrentVersion}).

%%%% BY ID AND CONDITION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fetch_if
	(
		atom(),
		ataxia_id:type(),
		ataxic:operation()
	)
	-> ({'ok', non_neg_integer(), any()} | ataxia_error:type()).
fetch_if (Client, DB, ID, Cond) ->
	CacheEntry = ataxia_cache_manager:request_cache_entry(Client, DB, ID),
	ataxia_cache_entry:request(Client, DB, ID, {fetch_if, DB, ID, Cond}).

-spec blind_update_if
	(
		atom(),
		ataxia_id:type(),
		ataxic:operation(),
		ataxic:operation()
	)
	-> ('ok' | ataxia_error:type()).
blind_update_if (Client, DB, ID, Cond, Op) ->
	CacheEntry = ataxia_cache_manager:request_cache_entry(Client, DB, ID),
	ataxia_cache_entry:request(Client, DB, ID, {blind_update_if, DB, ID, Cond, Op}).

-spec blind_update_if_then_fetch
	(
		atom(),
		ataxia_id:type(),
		ataxic:operation(),
		ataxic:operation()
	)
	-> ({'ok', non_neg_integer(), any()} | ataxia_error:type()).
blind_update_if_then_fetch (Client, DB, ID, Cond, Op) ->
	CacheEntry = ataxia_cache_manager:request_cache_entry(Client, DB, ID),
	ataxia_cache_entry:request(Client, DB, ID, {blind_update_if_then_fetch, DB, ID, Cond, Op}).

-spec blind_update_if_else_fetch
	(
		atom(),
		ataxia_id:type(),
		ataxic:operation(),
		ataxic:operation()
	)
	->
	(
		{'ok', non_neg_integer()}
		| {'ok', non_neg_integer(), any()}
		| ataxia_error:type()
	).
blind_update_if_else_fetch (Client, DB, ID, Cond, Op) ->
	CacheEntry = ataxia_cache_manager:request_cache_entry(Client, DB, ID),
	ataxia_cache_entry:request(Client, DB, ID, {blind_update_if_else_fetch, DB, ID, Cond, Op}).

-spec blind_remove_if
	(
		atom(),
		ataxia_id:type(),
		ataxic:operation()
	)
	-> ('ok' | ataxia_error:type()).
blind_remove_if (Client, DB, ID, Cond) ->
	CacheEntry = ataxia_cache_manager:request_cache_entry(Client, DB, ID),
	ataxia_cache_entry:request(Client, DB, ID, {blind_remove_if, DB, ID, Cond}).

%%%% ONE, BY CONDITION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fetch_any
	(
		atom(),
		ataxic:operation()
	)
	-> ({'ok', ataxia_id:type(), non_neg_integer(), any()} | ataxia_error:type()).
fetch_any (Client, DB, Condition) ->
	% TODO: Try all nodes one by one until one is found.
	unimplemented.

-spec blind_update_any
	(
		atom(),
		ataxic:operation(),
		ataxic:operation()
	)
	-> ({'ok', ataxia_id:type()} | ataxia_error:type()).
update_any (Client, DB, Condition, Op) ->
	% TODO: Try all nodes one by one until one is found.
	unimplemented.

-spec blind_update_then_fetch_any
	(
		atom(),
		ataxic:operation(),
		ataxic:operation()
	)
	->
	(
		{'ok', ataxia_id:type(), non_neg_integer(), any()}
		| ataxia_error:type()
	).
blind_update_then_fetch_any (Client, DB, Condition, Op) ->
	% TODO: Try all nodes one by one until one is found.
	unimplemented.

-spec blind_remove_any
	(
		atom(),
		ataxic:operation()
	)
	-> ({'ok', ataxia_id:type()} | ataxia_error:type()).
remove_any (Client, DB, Condition) ->
	% TODO: Try all nodes one by one until one is found.
	unimplemented.

%%%% ALL, BY CONDITION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fetch_all
	(
		atom(),
		ataxic:operation()
	)
	-> ({'ok', list({ataxia_id:type(), non_neg_integer(), any()})}).
fetch_all (Client, DB, Condition) ->
	% TODO: Try all nodes one by one, get all matching entries.
	unimplemented.

-spec blind_update_all
	(
		atom(),
		ataxic:operation(),
		ataxic:operation()
	)
	-> {'ok', list(ataxia_id:type())}.
update_all (Client, DB, Condition, Op) ->
	% TODO: Try all nodes one by one, apply to all the matching entries.
	unimplemented.

-spec blind_update_then_fetch_all
	(
		atom(),
		ataxic:operation(),
		ataxic:operation()
	)
	-> {'ok', list({ataxia_id:type(), non_neg_integer(), any()})}.
blind_update_then_fetch_all (Client, DB, Condition, Op) ->
	% TODO: Try all nodes one by one, apply to all the matching entries.
	unimplemented.

-spec blind_remove_all
	(
		atom(),
		ataxic:operation()
	)
	-> {'ok'}.
blind_remove_all (Client, DB, Condition) ->
	% TODO: Try all nodes one by one, apply to all the matching entries.
	unimplemented.
