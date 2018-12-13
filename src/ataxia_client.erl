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
      add/4,
      add_at/5,
      reserve/3,

      fetch/4,
      update/4,
      lock/4,
      remove/3,

      fetch_any/4,
      update_any/4,
      lock_any/4,
      remove_any/3,

      fetch_all/4,
      update_all/4,
      lock_all/4,
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

-spec get_db_node_for (binary()) -> node().
get_db_node_for (_ObjectID) ->
   get_debug_db_node().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec add_at
   (
      atom(),
      binary(),
      ataxia_security:permission(),
      ataxia_security:permission(),
      any()
   )
   -> ({'aborted', any()} | 'ok').
add_at (DB, ID, ReadPerm, WritePerm, Value) ->
   DBNode = get_db_node_for(ID),

   Reply =
      rpc:call
      (
         DBNode,
         atexia_server,
         add_at,
         [DB, ID, ReadPerm, WritePerm, Value]
      ),

   io:format
   (
      "~nataxia_client:add_at(~p) ! ~p -> ~p.~n",
      [{DB, ID, ReadPerm, WritePerm, Value}, DBNode, Reply]
   ),

   Reply.

-spec add
   (
      atom(),
      ataxia_security:permission(),
      ataxia_security:permission(),
      any()
   )
   -> ({'ok', ataxia_id:type()} | {'aborted', any()}).
add (DB, ReadPerm, WritePerm, Value) ->
   DBNode = get_random_db_node(),

   Reply =
      rpc:call(DBNode, atexia_server, add, [DB, ReadPerm, WritePerm, Value]),

   io:format
   (
      "~nataxia_client:add(~p) ! ~p -> ok.~n",
      [{DB, ReadPerm, WritePerm, Value}, DBNode]
   ),

   Reply.

-spec reserve
   (
      atom(),
      ataxia_security:user(),
      ataxia_id:type()
   )
   -> ('ok' | 'unavailable').
reserve (DB, User, ID) ->
   DBNode = get_db_node_for(ID),

   Reply = rpc:call(DBNode, atexia_server, reserve, [DB, User, ID]),

   io:format
   (
      "~nataxia_client:reserve(~p) ! ~p -> ~p.~n",
      [{DB, User, ID}, DBNode, Reply]
   ),

   Reply.


%%%% BY ID %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fetch
   (
      atom(),
      ataxia_security:user(),
      ataxic:type(),
      ataxia_id:type()
   )
   -> ({'ok', any()} | 'not_found').
fetch (DB, User, Selector, ID) ->
   DBNode = get_db_node_for(ID),

   Reply = rpc:call(DBNode, atexia_server, fetch, [DB, User, Selector, ID]),

   io:format
   (
      "~nataxia_client:fetch(~p) ! ~p -> ~p.~n",
      [{DB, User, Selector, ID}, DBNode, Reply]
   ),

   Reply.

-spec update
   (
      atom(),
      ataxia_security:user(),
      ataxiac:meta(),
      ataxia_id:type()
   )
   -> ('ok' | 'not_found').
update (DB, User, Op, ID) ->
   DBNode = get_db_node_for(ID),

   Reply = rpc:call(DBNode, atexia_server, update, [DB, User, Op, ID]),

   io:format
   (
      "~nataxia_client:update(~p) ! ~p -> ~p.~n",
      [{DB, User, Op, ID}, DBNode, Reply]
   ),

   Reply.

-spec lock
   (
      atom(),
      ataxia_security:user(),
      ataxia_time:type(),
      ataxia_id:type()
   )
   -> ('ok' | 'not_found').
lock (DB, User, Duration, ID) ->
   DBNode = get_db_node_for(ID),

   Reply = rpc:call(DBNode, atexia_server, lock, [DB, User, Duration, ID]),

   io:format
   (
      "~nataxia_client:lock(~p) ! ~p -> ~p.~n",
      [{DB, User, Duration, ID}, DBNode, Reply]
   ),

   Reply.


-spec remove
   (
      atom(),
      ataxia_security:user(),
      ataxia_id:type()
   )
   -> ('ok' | 'not_found').
remove (DB, User, ID) ->
   DBNode = get_db_node_for(ID),

   Reply = rpc:call(DBNode, atexia_server, remove, [DB, User, ID]),

   io:format
   (
      "~nataxia_client:remove(~p) ! ~p -> ~p.~n",
      [{DB, User, ID}, DBNode, Reply]
   ),

   Reply.

%%%% ONE, BY CONDITION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fetch_any
   (
      atom(),
      ataxia_security:user(),
      ataxic:type(),
      ataxic:meta()
   )
   -> ({'ok', ataxia_id:type(), any()} | 'not_found').
fetch_any (DB, User, Selector, Condition) ->
   % TODO: Try all nodes one by one until one is found.
   DBNode = get_db_node_for(<<"">>),

   Reply =
      rpc:call
      (
         DBNode,
         atexia_server,
         fetch_any,
         [DB, User, Selector, Condition]
      ),

   io:format
   (
      "~nataxia_client:fetch_any(~p) ! ~p -> ~p.~n",
      [{DB, User, Selector, Condition}, DBNode, Reply]
   ),

   Reply.

-spec update_any
   (
      atom(),
      ataxia_security:user(),
      ataxiac:meta(),
      ataxiac:meta()
   )
   -> ({'ok', ataxia_id:type()} | 'not_found').
update_any (DB, User, Op, Condition) ->
   % TODO: Try all nodes one by one until one is found.
   DBNode = get_db_node_for(<<"">>),

   Reply =
      rpc:call(DBNode, atexia_server, update_any, [DB, User, Op, Condition]),

   io:format
   (
      "~nataxia_client:update(~p) ! ~p -> ~p.~n",
      [{DB, User, Op, Condition}, DBNode, Reply]
   ),

   Reply.

-spec lock_any
   (
      atom(),
      ataxia_security:user(),
      ataxia_time:type(),
      ataxic:meta()
   )
   -> ({'ok', ataxia_id:type()} | 'not_found').
lock_any (DB, User, Duration, Condition) ->
   % TODO: Try all nodes one by one until one is found.
   DBNode = get_db_node_for(<<"">>),

   Reply =
      rpc:call
      (
         DBNode,
         atexia_server,
         lock_any,
         [DB, User, Duration, Condition]
      ),

   io:format
   (
      "~nataxia_client:lock_any(~p) ! ~p -> ~p.~n",
      [{DB, User, Duration, Condition}, DBNode, Reply]
   ),

   Reply.

-spec remove_any
   (
      atom(),
      ataxia_security:user(),
      ataxic:meta()
   )
   -> ({'ok', ataxia_id:type()} | 'not_found').
remove_any (DB, User, Condition) ->
   % TODO: Try all nodes one by one until one is found.
   DBNode = get_db_node_for(<<"">>),

   Reply = rpc:call(DBNode, atexia_server, remove_any, [DB, User, Condition]),

   io:format
   (
      "~nataxia_client:remove(~p) ! ~p -> ~p.~n",
      [{DB, User, Condition}, DBNode, Reply]
   ),

   Reply.

%%%% ALL, BY CONDITION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fetch_all
   (
      atom(),
      ataxia_security:user(),
      ataxic:type(),
      ataxic:meta()
   )
   -> ({'ok', list({ataxia_id:type(), any()})}).
fetch_all (DB, User, Selector, Condition) ->
   % TODO: Try all nodes one by one, apply to all they matching items.
   DBNode = get_db_node_for(<<"">>),

   Reply =
      rpc:call
      (
         DBNode,
         atexia_server,
         fetch_all,
         [DB, User, Selector, Condition]
      ),

   io:format
   (
      "~nataxia_client:fetch_all(~p) ! ~p -> ~p.~n",
      [{DB, User, Selector, Condition}, DBNode, Reply]
   ),

   Reply.

-spec update_all
   (
      atom(),
      ataxia_security:user(),
      ataxiac:meta(),
      ataxiac:meta()
   )
   -> {'ok', list(ataxia_id:type())}.
update_all (DB, User, Op, Condition) ->
   % TODO: Try all nodes one by one, apply to all they matching items.
   DBNode = get_db_node_for(<<"">>),

   Reply =
      rpc:call(DBNode, atexia_server, update_all, [DB, User, Op, Condition]),

   io:format
   (
      "~nataxia_client:update(~p) ! ~p -> ~p.~n",
      [{DB, User, Op, Condition}, DBNode, Reply]
   ),

   Reply.

-spec lock_all
   (
      atom(),
      ataxia_security:user(),
      ataxia_time:type(),
      ataxic:meta()
   )
   -> {'ok', list(ataxia_id:type())}.
lock_all (DB, User, Duration, Condition) ->
   % TODO: Try all nodes one by one, apply to all they matching items.
   DBNode = get_db_node_for(<<"">>),

   Reply =
      rpc:call
      (
         DBNode,
         atexia_server,
         lock_all,
         [DB, User, Duration, Condition]
      ),

   io:format
   (
      "~nataxia_client:lock_all(~p) ! ~p -> ~p.~n",
      [{DB, User, Duration, Condition}, DBNode, Reply]
   ),

   Reply.

-spec remove_all
   (
      atom(),
      ataxia_security:user(),
      ataxic:meta()
   )
   -> {'ok', list(ataxia_id:type())}.
remove_all (DB, User, Condition) ->
   % TODO: Try all nodes one by one, apply to all they matching items.
   DBNode = get_db_node_for(<<"">>),

   Reply = rpc:call(DBNode, atexia_server, remove_all, [DB, User, Condition]),

   io:format
   (
      "~nataxia_client:remove(~p) ! ~p -> ~p.~n",
      [{DB, User, Condition}, DBNode, Reply]
   ),

   Reply.

