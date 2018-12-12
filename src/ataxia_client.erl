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
      insert_at/5,
      insert/4,
      remove/3,
      fetch/4,
      reserve/3,
      commit/4
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
-spec insert_at
   (
      atom(),
      binary(),
      ataxia_security:permission(),
      ataxia_security:permission(),
      any()
   )
   -> ({'aborted', any()} | 'ok').
insert_at (DB, ID, ReadPerm, WritePerm, Value) ->
   DBNode = get_db_node_for(ID),

   Reply =
      rpc:call
      (
         DBNode,
         atexia_server,
         insert_at,
         [DB, ID, ReadPerm, WritePerm, Value]
      ),

   io:format
   (
      "~nshr_database:insert_at(~p) ! ~p -> ~p.~n",
      [{DB, ID, ReadPerm, WritePerm, Value}, DBNode, Reply]
   ),

   Reply.

-spec insert
   (
      atom(),
      ataxia_security:permission(),
      ataxia_security:permission(),
      any()
   )
   -> ({'ok', ataxia_id:type()} | {'aborted', any()}).
insert (DB, ReadPerm, WritePerm, Value) ->
   DBNode = get_random_db_node(),

   Reply =
      rpc:call(DBNode, atexia_server, insert, [DB, ReadPerm, WritePerm, Value]),

   io:format
   (
      "~nshr_database:insert(~p) ! ~p -> ok.~n",
      [{DB, ReadPerm, WritePerm, Value}, DBNode]
   ),

   Reply.

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

   Reply = rpc:call(DBNode, atexia_server, read, [DB, User, Selector, ID]),

   io:format
   (
      "~nshr_database:fetch(~p) ! ~p -> ~p.~n",
      [{DB, User, Selector, ID}, DBNode, Reply]
   ),

   Reply.

-spec commit
   (
      atom(),
      ataxia_security:user(),
      ataxiac:meta(),
      ataxia_id:type()
   )
   -> ('ok' | 'not_found').
commit (DB, User, Op, ID) ->
   DBNode = get_db_node_for(ID),

   Reply = rpc:call(DBNode, atexia_server, query, [DB, User, Op, ID]),

   io:format
   (
      "~nataxia_client:commit(~p) ! ~p -> ~p.~n",
      [{DB, User, Op, ID}, DBNode, Reply]
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
