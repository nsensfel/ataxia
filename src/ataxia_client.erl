-module(ataxia_client).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ADD NEW ELEMENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%
%%%% Providing everything
%%%%
-spec add_at (atom(), any(), ataxia_id:type()) -> ('ok' | ataxia_error:type()).
add_at (DB, Value, ID) ->
	DBNode = get_db_node_for(ID),
	rpc:call(DBNode, ataxia_server, add_at, [DB, Value, ID]).

-spec add (atom(), any()) -> ({'ok', ataxia_id:type()} | ataxia_error:type()).
add (DB, Value) ->
	DBNode = get_random_db_node(),
	rpc:call ( DBNode, ataxia_server, add, [DB, Value]).

-spec reserve (atom()) -> ({'ok', ataxia_id:type()} | ataxia_error:type()).
reserve (DB, ReadPerm, WritePerm, Lock) ->
	DBNode = get_random_db_node(),
	rpc:call(DBNode, ataxia_server, reserve, [DB]).

-spec reserve_at (atom(), ataxia_id:type()) -> ('ok' | ataxia_error:type()).
reserve_at (DB, ID) ->
	DBNode = get_db_node_for(ID),
	rpc:call(DBNode, ataxia_server, reserve_at, [DB, ID]).


%%%% BY ID %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fetch (atom(), ataxia_id:type()) -> ({'ok', any()} | ataxia_error:type()).
fetch (DB, ID) ->
	DBNode = get_db_node_for(ID),
	rpc:call(DBNode, ataxia_server, fetch, [DB, ID]).

-spec update
	(
		atom(),
		ataxic:meta(),
		ataxia_id:type()
	)
	-> ('ok' | ataxia_error:type()).
update (DB, Op, ID) ->
	DBNode = get_db_node_for(ID),
	rpc:call(DBNode, ataxia_server, update, [DB, Op, ID]).

-spec update_and_fetch
	(
		atom(),
		ataxic:meta(),
		ataxia_id:type()
	)
	-> ({'ok', any()} | ataxia_error:type()).
update_and_fetch (DB, Op, ID) ->
	DBNode = get_db_node_for(ID),
	rpc:call(DBNode, ataxia_server, update_and_fetch, [DB, Op, ID]).

-spec remove (atom(), ataxia_id:type()) -> ('ok' | ataxia_error:type()).
remove (DB, ID) ->
	DBNode = get_db_node_for(ID),
	rpc:call(DBNode, ataxia_server, remove, [DB, ID]).

%%%% BY ID AND CONDITION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fetch_if
	(
		atom(),
		ataxia_id:type(),
		ataxic:basic()
	)
	-> ({'ok', any()} | ataxia_error:type()).
fetch_if (DB, ID, Cond) ->
	DBNode = get_db_node_for(ID),
	rpc:call(DBNode, ataxia_server, fetch_if, [DB, ID, Cond]).

-spec update_if
	(
		atom(),
		ataxic:meta(),
		ataxia_id:type(),
		ataxic:basic()
	)
	-> ('ok' | ataxia_error:type()).
update_if (DB, Op, ID, Cond) ->
	DBNode = get_db_node_for(ID),
	rpc:call(DBNode, ataxia_server, update_if, [DB, Op, ID, Cond]).

-spec update_and_fetch_if
	(
		atom(),
		ataxic:meta(),
		ataxia_id:type(),
		ataxic:basic()
	)
	-> ({'ok', any()} | ataxia_error:type()).
update_and_fetch_if (DB, Op, ID, Cond) ->
	DBNode = get_db_node_for(ID),
	rpc:call
	(
		DBNode,
		ataxia_server,
		update_and_fetch_if,
		[DB, Op, ID, Cond]
	).

-spec update_if_else_fetch
	(
		atom(),
		ataxic:meta(),
		ataxia_id:type(),
		ataxic:basic()
	)
	-> ({'ok', any()} | ataxia_error:type()).
update_and_fetch_if (DB, Op, ID, Cond) ->
	DBNode = get_db_node_for(ID),
	rpc:call
	(
		DBNode,
		ataxia_server,
		update_if_else_fetch,
		[DB, Op, ID, Cond]
	).

-spec remove_if
	(
		atom(),
		ataxia_id:type(),
		ataxic:basic()
	)
	-> ('ok' | ataxia_error:type()).
remove_if (DB, ID, Cond) ->
	DBNode = get_db_node_for(ID),
	rpc:call(DBNode, ataxia_server, remove_if, [DB, ID, Cond]).

%%%% ONE, BY CONDITION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fetch_any
	(
		atom(),
		ataxic:basic()
	)
	-> ({'ok', ataxia_id:type(), any()} | ataxia_error:type()).
fetch_any (DB, Condition) ->
	% TODO: Try all nodes one by one until one is found.
	DBNode = get_db_node_for(<<"">>),
	rpc:call(DBNode, ataxia_server, fetch_any, [DB, Condition]).

-spec update_any
	(
		atom(),
		ataxic:meta(),
		ataxic:basic()
	)
	-> ({'ok', ataxia_id:type()} | ataxia_error:type()).
update_any (DB, Op, Condition) ->
	% TODO: Try all nodes one by one until one is found.
	DBNode = get_db_node_for(<<"">>),
	rpc:call(DBNode, ataxia_server, update_any, [DB, Op, Condition]).

-spec update_and_fetch_any
	(
		atom(),
		ataxic:meta(),
		ataxic:basic()
	)
	-> ({'ok', any(), ataxia_id:type()} | ataxia_error:type()).
update_and_fetch_any (DB, Op, Condition) ->
	% TODO: Try all nodes one by one until one is found.
	DBNode = get_db_node_for(<<"">>),

	rpc:call
	(
		DBNode,
		ataxia_server,
		update_and_fetch_any,
		[DB, Op, Condition]
	).


-spec remove_any
	(
		atom(),
		ataxic:basic()
	)
	-> ({'ok', ataxia_id:type()} | ataxia_error:type()).
remove_any (DB, Condition) ->
	% TODO: Try all nodes one by one until one is found.
	DBNode = get_db_node_for(<<"">>),

	rpc:call(DBNode, ataxia_server, remove_any, [DB, Condition]).

%%%% ALL, BY CONDITION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fetch_all
	(
		atom(),
		ataxic:basic()
	)
	-> ({'ok', list({ataxia_id:type(), any()})}).
fetch_all (DB, Condition) ->
	% TODO: Try all nodes one by one, apply to all they matching items.
	DBNode = get_db_node_for(<<"">>),

	rpc:call( DBNode, ataxia_server, fetch_all, [DB, Condition]).

-spec update_all
	(
		atom(),
		ataxic:meta(),
		ataxic:basic()
	)
	-> {'ok', list(ataxia_id:type())}.
update_all (DB, Op, Condition) ->
	% TODO: Try all nodes one by one, apply to all they matching items.
	DBNode = get_db_node_for(<<"">>),
	rpc:call(DBNode, ataxia_server, update_all, [DB, Op, Condition]).

-spec update_and_fetch_all
	(
		atom(),
		ataxic:meta(),
		ataxic:basic()
	)
	-> {'ok', list({any(), ataxia_id:type()})}.
update_and_fetch_all (DB, Op, Condition) ->
	% TODO: Try all nodes one by one, apply to all they matching items.
	DBNode = get_db_node_for(<<"">>),

	rpc:call
	(
		DBNode,
		ataxia_server,
		update_and_fetch_all,
		[DB, Op, Condition]
	).

-spec remove_all
	(
		atom(),
		ataxic:basic()
	)
	-> {'ok', list(ataxia_id:type())}.
remove_all (DB, Condition) ->
	% TODO: Try all nodes one by one, apply to all they matching items.
	DBNode = get_db_node_for(<<"">>),

	rpc:call(DBNode, ataxia_server, remove_all, [DB, Condition]).
