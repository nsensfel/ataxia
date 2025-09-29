-module(ataxia_client).
%%%% This should be the main client interface, handling cache and locks.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TIMEOUT, 100000).

-record
(
	client,
	{
		known_cache_entries :: #{{node(), ataxia_id:type()} => pid()},
		cache_managers :: ataxia_cache_manager:collection(),
		next_request_id :: non_neg_integer()
	}
).

-type type() :: #client{}.

-export_type([type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FIXME: needs sqfe_update_else_fetch, to retry upon failure.
%% FIXME: needs lock handling. How do you cancel a pending lock request, though?
%% Release?
%% Client gets a lock, then performs requests, then releases it.
%% Getting multiple locks should be done in {DB, ID} alphabetical order.
-export
(
	[
		start_node_processes/0,
		new/0,
		merge/2 % merges 2 clients from 2 asynchronous calls.
	]
).

-export
(
	[
		add/4,
		add_at/5,

		fetch/4,
		fetch_if_new/6,
		blind_update/5,
		safe_update/2,
		safe_update/7,
		blind_update_then_fetch/5,
		blind_remove/4,
		safe_remove/5,

		fetch_if/5,
		blind_update_if/6,
		blind_update_if_then_fetch/6,
		blind_update_if_else_fetch/6,
		blind_remove_if/5,

		release_lock/1
		% TODO any_where, all_where variants.
%		fetch_any/3,
%		update_any/4,
%		update_and_fetch_any/4,
%		remove_any/3,
%
%		fetch_all/3,
%		update_all/4,
%		update_and_fetch_all/4,
%		remove_all/3
	]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec await_reply
(
	type(),
	atom(),
	ataxia_id:type(),
	any(),
	pid(),
	non_neg_integer()
)
-> {type(), any()}.
await_reply (Client, DB, ID, Request, EntryPID, RequestID) ->
	receive
		{ataxia_reply, ReplyToID, Reply} when ReplyToID == RequestID ->
			{Client, Reply}
	after ?TIMEOUT ->
		erlang:display({"Cache entry answer timed out", DB, ID}),
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
	NewEntryPID =
		ataxia_cache_manager:request_entry_for
		(
			Client#client.cache_managers,
			DB,
			ID,
			EntryPID
		),
	case is_process_alive(NewEntryPID) of
		true ->
			ataxia_cache_entry:request(NewEntryPID, 0, Request),
			await_reply
			(
				Client#client
				{
					known_cache_entries =
						maps:update
						(
							{DB, ID},
							NewEntryPID,
							Client#client.known_cache_entries
						)
				},
				DB,
				ID,
				Request,
				NewEntryPID,
				0
			);

		false ->
			erlang:display({"Known cache entry is down", DB, ID}),
			request_new_handler_of(Client, DB, ID, Request, NewEntryPID)
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
	case maps:find({DB, ID}, Client#client.known_cache_entries) of
		{ok, EntryPID} ->
			ataxia_cache_entry:request(EntryPID, 0, Request),
			case is_process_alive(EntryPID) of
				true -> await_reply(Client, DB, ID, Request, EntryPID, 0);
				false -> request_new_handler_of(Client, DB, ID, Request, EntryPID)
			end;

		error -> request_new_handler_of(Client, DB, ID, Request, none)
	end.

-spec remove_cache_entry ({atom(), ataxia_id:type()}, type()) -> type().
remove_cache_entry (Key, Client) ->
	Client#client
	{
		known_cache_entries = maps:remove(Key, Client#client.known_cache_entries)
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_node_processes () -> 'ok'.
start_node_processes () ->
	ataxia_cache_manager:start_collection(),
	ok.

-spec new () -> type().
new () ->
	#client
	{
		known_cache_entries = maps:new(),
		cache_managers = ataxia_cache_manager:get_collection(),
		next_request_id = 0
	}.

-spec merge (type(), type()) -> type().
merge (ClientA, ClientB) ->
	ClientA#client
	{
		known_cache_entries =
			maps:merge
			(
				maps:filter
				(
					fun (_Key, Value) -> Value /= none end,
					ClientA#client.known_cache_entries
				),
				maps:filter
				(
					fun (_Key, Value) -> Value /= none end,
					ClientB#client.known_cache_entries
				)
			)
	}.

%%%% ADD NEW ELEMENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Providing everything
%%%%
-spec add_at
	(
		type(),
		atom(),
		ataxia_id:type(),
		ataxia_lock:message(),
		any()
	)
	->
	{
		type(),
		(
			{'ok', non_neg_integer()}
			| {'ok', ataxia_network:proc(), non_neg_integer()}
			| ataxia_error:type()
		)
	}.
add_at (Client, DB, ID, Lock, Value) ->
	request(Client, DB, ID, {add_at, DB, ID, Lock, Value}).

-spec add
	(
		type(),
		atom(),
		ataxia_lock:message(),
		any()
	)
	->
	{
		type(),
		(
			{'ok', ataxia_id:type(), non_neg_integer()}
			| {'ok', ataxia_id:type(), ataxia_network:proc(), non_neg_integer()}
			| ataxia_error:type()
		)
	}.
add (S0Client, _DB, read, _Value) -> {S0Client, {error, lock}};
add (S0Client, _DB, {temp, read}, _Value) -> {S0Client, {error, lock}};
add (S0Client, DB, Lock, Value) ->
	erlang:display("Add: BUTF..."),
	{ S1Client, Result } =
		blind_update_then_fetch
		(
			S0Client,
			DB,
			ataxia_id:table_manager(),
			{temp, write},
			ataxia_table_manager:ataxic_generate_id()
		),
	erlang:display("Add: AddAt..."),
	case Result of
		{ok, _Version, TableManager} ->
			NewID = ataxia_table_manager:get_last_id(TableManager),
			case add_at(S1Client, DB, NewID, Lock, Value) of
				{S2Client, {ok, Version}} -> {S2Client, {ok, NewID, Version}};
				{S2Client, {ok, LockReply, Version}} ->
					{S2Client, {ok, NewID, LockReply, Version}};

				Other -> Other
			end;

		Error -> erlang:display({add, error, Error}), {S1Client, Error}
	end.

%%%% BY ID %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fetch
	(
		type(),
		atom(),
		ataxia_id:type(),
		ataxia_lock:message()
	)
	->
	{
		type(),
		(
			{'ok', non_neg_integer(), any()}
			|
			{
				'ok',
				ataxia_network:proc(),
				non_neg_integer(),
				any()
			}
			| ataxia_error:type()
		)
	}.
fetch (Client, DB, ID, Lock) ->
	request(Client, DB, ID, {fetch, DB, ID, Lock}).

-spec fetch_if_new
	(
		type(),
		atom(),
		ataxia_id:type(),
		ataxia_lock:message(),
		non_neg_integer(),
		any()
	)
	->
	{
		type(),
		(
			'ok'
			| {'ok', ataxia_network:proc()}
			| {'ok', non_neg_integer(), any()}
			|
			{
				'ok',
				ataxia_network:proc(),
				non_neg_integer(),
				any()
			}
			| ataxia_error:type()
		)
	}.
fetch_if_new (Client, DB, ID, Lock, Version, Value) ->
	request(Client, DB, ID, {fetch_if_new, DB, ID, Lock, Version, Value}).

-spec blind_update
	(
		type(),
		atom(),
		ataxia_id:type(),
		ataxia_lock:message(),
		ataxic:type()
	)
	->
	{
		type(),
		(
			{'ok', non_neg_integer()}
			| {'ok', ataxia_network:proc(), non_neg_integer()}
			| ataxia_error:type()
		)
	}.
blind_update (Client, DB, ID, Lock, Op) ->
	request(Client, DB, ID, {blind_update, DB, ID, Lock, Op}).

-spec safe_update
	(
		type(),
		atom(),
		ataxia_id:type(),
		ataxia_lock:message(),
		ataxic:type(),
		non_neg_integer(),
		any()
	)
	->
	{
		type(),
		(
			{'ok', non_neg_integer()}
			| {'ok', ataxia_network:proc(), non_neg_integer()}
			| ataxia_error:type()
		)
	}.
safe_update
(
	Client,
	DB,
	ID,
	Lock,
	Op,
	ExpectedCurrentVersion,
	ExpectedNewValue
) ->
	request
	(
		Client,
		DB,
		ID,
		{safe_update, DB, ID, Lock, Op, ExpectedCurrentVersion, ExpectedNewValue}
	).

-spec safe_update (type(), ataxia_client_data:type())
	->
	{
		type(),
		(
			{'ok', non_neg_integer()}
			| {'ok', ataxia_network:proc(), non_neg_integer()}
			| ataxia_error:type()
		)
	}.
safe_update (Client, AtaxiaClientData) ->
	safe_update
	(
		Client,
		ataxia_client_data:get_database(AtaxiaClientData),
		ataxia_client_data:get_id(AtaxiaClientData),
		ataxia_client_data:get_lock(AtaxiaClientData),
		ataxic_optimize:aggressive(ataxia_client_data:get_ataxic(AtaxiaClientData)),
		ataxia_client_data:get_version(AtaxiaClientData),
		ataxia_client_data:get_value(AtaxiaClientData)
	).

-spec blind_update_then_fetch
	(
		type(),
		atom(),
		ataxia_id:type(),
		ataxia_lock:message(),
		ataxic:type()
	)
	->
	{
		type(),
		(
			{'ok', non_neg_integer(), any()}
			|
			{
				'ok',
				ataxia_network:proc(),
				non_neg_integer(),
				any()
			}
			| ataxia_error:type()
		)
	}.
blind_update_then_fetch (Client, DB, ID, Lock, Op) ->
	erlang:display("BUTF: client call..."),
	request
	(
		Client,
		DB,
		ID,
		{blind_update_then_fetch, DB, ID, Lock, Op}
	).

-spec blind_remove
	(
		type(),
		atom(),
		ataxia_lock:message(),
		ataxia_id:type()
	) -> {type(), ('ok' | ataxia_error:type())}.
blind_remove (Client, DB, Lock, ID) ->
	{NewClient, Reply} =
		request(Client, DB, ID, {blind_remove, DB, ID, Lock}),
	{
		remove_cache_entry({DB, ID}, NewClient),
		Reply
	}.

-spec safe_remove
	(
		type(),
		atom(),
		ataxia_id:type(),
		ataxia_lock:message(),
		non_neg_integer()
	)
	-> {type(), ('ok' | ataxia_error:type())}.
safe_remove (Client, DB, ID, Lock, ExpectedCurrentVersion) ->
	{NewClient, Reply} =
		request
		(
			Client,
			DB,
			ID,
			{safe_remove, DB, ID, Lock, ExpectedCurrentVersion}
		),

	{
		remove_cache_entry({DB, ID}, NewClient),
		Reply
	}.

%%%% BY ID AND CONDITION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fetch_if
	(
		type(),
		atom(),
		ataxia_id:type(),
		ataxia_lock:message(),
		ataxic:type()
	)
	->
	{
		type(),
		(
			{'ok', non_neg_integer(), any()}
			|
			{
				'ok',
				ataxia_network:proc(),
				non_neg_integer(),
				any()
			}
			| ataxia_error:type()
		)
	}.
fetch_if (Client, DB, ID, Lock, Cond) ->
	request(Client, DB, ID, {fetch_if, DB, ID, Lock, Cond}).

-spec blind_update_if
	(
		type(),
		atom(),
		ataxia_id:type(),
		ataxia_lock:message(),
		ataxic:type(),
		ataxic:type()
	)
	->
	{
		type(),
		(
			{'ok', non_neg_integer()}
			| {'ok', ataxia_network:proc(), non_neg_integer()}
			| ataxia_error:type()
		)
	}.
blind_update_if (Client, DB, ID, Lock, Cond, Op) ->
	request(Client, DB, ID, {blind_update_if, DB, ID, Lock, Cond, Op}).

-spec blind_update_if_then_fetch
	(
		type(),
		atom(),
		ataxia_id:type(),
		ataxia_lock:message(),
		ataxic:type(),
		ataxic:type()
	)
	->
	{
		type(),
		(
			{'ok', non_neg_integer(), any()}
			|
			{
				'ok',
				ataxia_network:proc(),
				non_neg_integer(),
				any()
			}
			| ataxia_error:type()
		)
	}.
blind_update_if_then_fetch (Client, DB, ID, Lock, Cond, Op) ->
	request
	(
		Client,
		DB,
		ID,
		{blind_update_if_then_fetch, DB, ID, Lock, Cond, Op}
	).

-spec blind_update_if_else_fetch
	(
		type(),
		atom(),
		ataxia_id:type(),
		ataxia_lock:message(),
		ataxic:type(),
		ataxic:type()
	)
	->
	{
		type(),
		(
			{
				'ok',
				'updated',
				ataxia_network:proc(),
				non_neg_integer()
			}
			| {'ok', 'updated', non_neg_integer()}
			| {'ok', 'fetch', non_neg_integer(), any()}
			|
			{
				'ok',
				'fetch',
				ataxia_network:proc(),
				non_neg_integer(),
				any()
			}
			| ataxia_error:type()
		)
	}.
blind_update_if_else_fetch (Client, DB, ID, Lock, Cond, Op) ->
	request
	(
		Client,
		DB,
		ID,
		{blind_update_if_else_fetch, DB, ID, Lock, Cond, Op}
	).

-spec blind_remove_if
	(
		type(),
		atom(),
		ataxia_id:type(),
		ataxia_lock:message(),
		ataxic:type()
	)
	-> {type(), ('ok' | ataxia_error:type())}.
blind_remove_if (Client, DB, ID, Lock, Cond) ->
	{NewClient, Reply} =
		request(Client, DB, ID, {blind_remove_if, DB, ID, Lock, Cond}),

	{
		remove_cache_entry({DB, ID}, NewClient),
		Reply
	}.

%%%% ONE, BY CONDITION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-spec fetch_any
%	(
%		atom(),
%		ataxic:type()
%	)
%	-> ({'ok', ataxia_id:type(), non_neg_integer(), any()} | ataxia_error:type()).
%fetch_any (Client, DB, Condition) ->
%	% TODO: Try all nodes one by one until one is found.
%	unimplemented.
%
%-spec blind_update_any
%	(
%		atom(),
%		ataxic:type(),
%		ataxic:type()
%	)
%	-> ({'ok', ataxia_id:type()} | ataxia_error:type()).
%update_any (Client, DB, Condition, Op) ->
%	% TODO: Try all nodes one by one until one is found.
%	unimplemented.
%
%-spec blind_update_then_fetch_any
%	(
%		atom(),
%		ataxic:type(),
%		ataxic:type()
%	)
%	->
%	(
%		{'ok', ataxia_id:type(), non_neg_integer(), any()}
%		| ataxia_error:type()
%	).
%blind_update_then_fetch_any (Client, DB, Condition, Op) ->
%	% TODO: Try all nodes one by one until one is found.
%	unimplemented.
%
%-spec blind_remove_any
%	(
%		atom(),
%		ataxic:type()
%	)
%	-> ({'ok', ataxia_id:type()} | ataxia_error:type()).
%remove_any (Client, DB, Condition) ->
%	% TODO: Try all nodes one by one until one is found.
%	unimplemented.
%
%%%% ALL, BY CONDITION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-spec fetch_all
%	(
%		atom(),
%		ataxic:type()
%	)
%	-> ({'ok', list({ataxia_id:type(), non_neg_integer(), any()})}).
%fetch_all (Client, DB, Condition) ->
%	% TODO: Try all nodes one by one, get all matching entries.
%	unimplemented.
%
%-spec blind_update_all
%	(
%		atom(),
%		ataxic:type(),
%		ataxic:type()
%	)
%	-> {'ok', list(ataxia_id:type())}.
%update_all (Client, DB, Condition, Op) ->
%	% TODO: Try all nodes one by one, apply to all the matching entries.
%	unimplemented.
%
%-spec blind_update_then_fetch_all
%	(
%		atom(),
%		ataxic:type(),
%		ataxic:type()
%	)
%	-> {'ok', list({ataxia_id:type(), non_neg_integer(), any()})}.
%blind_update_then_fetch_all (Client, DB, Condition, Op) ->
%	% TODO: Try all nodes one by one, apply to all the matching entries.
%	unimplemented.
%
%-spec blind_remove_all
%	(
%		atom(),
%		ataxic:type()
%	)
%	-> {'ok'}.
%blind_remove_all (Client, DB, Condition) ->
%	% TODO: Try all nodes one by one, apply to all the matching entries.
%	unimplemented.
-spec release_lock (ataxia_network:proc()) -> 'ok'.
release_lock ({LockNode, LockPID}) ->
	erpc:cast(LockNode, ataxia_lock, release_lock, [LockPID, {node(), self()}]).
