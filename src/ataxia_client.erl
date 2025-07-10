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
		known_cache_entries :: dict:dict({node(), pid()}, pid()),
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
		blind_update/5,
		safe_update/7,
		blind_update_then_fetch/5,
		blind_remove/4,
		safe_remove/5,

		fetch_if/5,
		blind_update_if/6,
		blind_update_if_then_fetch/6,
		blind_update_if_else_fetch/6,
		blind_remove_if/5

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
	ataxia_cache_entry:request(NewEntryPID, 0, Request),
	case is_process_alive(NewEntryPID) of
		true ->
			await_reply
			(
				Client#client
				{
					known_cache_entries =
						dict:store
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
	case dict:find({DB, ID}, Client#client.known_cache_entries) of
		{ok, EntryPID} ->
			ataxia_cache_entry:request(EntryPID, 0, Request),
			case is_process_alive(EntryPID) of
				true -> await_reply(Client, DB, ID, Request, EntryPID, 0);
				false -> request_new_handler_of(Client, DB, ID, Request, EntryPID)
			end;

		error -> request_new_handler_of(Client, DB, ID, Request, none)
	end.

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
	ataxia_cache_manager:peek_entry_for(Client#client.cache_managers, DB, ID).

-spec remove_cache_entry ({atom(), ataxia_id:type()}, type()) -> type().
remove_cache_entry (Key, Client) ->
	Client#client
	{
		known_cache_entries = dict:erase(Key, Client#client.known_cache_entries)
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new () -> type().
new () ->
	#client
	{
		known_cache_entries = dict:new(),
		cache_managers = ataxia_cache_manager:get_collection(),
		next_request_id = 0
	}.

-spec merge (type(), type()) -> type().
merge (ClientA, ClientB) ->
	ClientA#client
	{
		known_cache_entries =
			dict:filter
			(
				fun (_Key, Value) -> Value /= none end,
				dict:merge
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
			| {'ok', ataxia_lock_janitor:lock_reference(), non_neg_integer()}
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
			{'ok', ataxia_id:type()}
			| {'ok', ataxia_lock_janitor:lock_reference(), ataxia_id:type()}
			| ataxia_error:type()
		)
	}.
add (Client, DB, Lock, Value) ->
	request(Client, DB, ataxia_id:table_manager(), {add, DB, Lock, Value}).

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
				ataxia_lock_janitor:lock_reference(),
				non_neg_integer(),
				any()
			}
			| ataxia_error:type()
		)
	}.
fetch (Client, DB, ID, Lock) ->
	request(Client, DB, ID, {fetch, DB, ID, Lock}).

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
			| {'ok', ataxia_lock_janitor:lock_reference(), non_neg_integer()}
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
			| {'ok', ataxia_lock_janitor:lock_reference(), non_neg_integer()}
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
				ataxia_lock_janitor:lock_reference(),
				non_neg_integer(),
				any()
			}
			| ataxia_error:type()
		)
	}.
blind_update_then_fetch (Client, DB, ID, Lock, Op) ->
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
				ataxia_lock_janitor:lock_reference(),
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
			| {'ok', ataxia_lock_janitor:lock_reference(), non_neg_integer()}
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
				ataxia_lock_janitor:lock_reference(),
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
				ataxia_lock_janitor:lock_reference(),
				non_neg_integer()
			}
			| {'ok', 'updated', non_neg_integer()}
			| {'ok', 'fetch', non_neg_integer(), any()}
			|
			{
				'ok',
				'fetch',
				ataxia_lock_janitor:lock_reference(),
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
