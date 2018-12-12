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
      read/4,
      remove/3,
      reserve/3,
      insert_at/5,
      insert/4,
      query/1
   ]
).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec query_transaction (shr_db_query:type()) -> 'ok'.
query_transaction (Query) ->
   DB = shr_db_query:get_database(Query),
   ID = shr_db_query:get_entry_id(Query),
   [Item] = mnesia:read(DB, ID),
   {ok, UpdatedItem} = shr_db_query:apply_to(Query, Item),

   mnesia:write(DB, UpdatedItem, sticky_write),

   ok.

-spec add_new_item (atom(), ataxia_entry:type()) -> 'ok'.
add_new_item (DB, Item) ->
   ID = ataxia_entry:get_id(Item),
   [] = mnesia:read(DB, ID),

   mnesia:write(DB, Item, sticky_write),

   ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec read
   (
      atom(),
      ataxic:type(),
      binary(),
      ataxia_security:user()
   )
   -> ({'aborted', any()} | {'ok', any()} | 'not_found').
read (ID, Selector, User, DB) ->
   case mnesia:transaction(fun mnesia:read/2, [DB, ID]) of
      {'atomic', []} -> 'not_found';
      {'atomic', [Item]} ->
         true =
            ataxia_security:can_access
            (
               User,
               ataxia_entry:get_read_permission(Item)
            ),
         {ok, ataxic:apply_to(Selector, ataxia_entry:get_value(Item))};

      Other -> {'aborted', Other}
   end.

-spec insert_at
   (
      atom(),
      binary(),
      ataxia_security:permission(),
      ataxia_security:permission(),
      any())
   -> ({'aborted', any()} | 'ok').
insert_at (DB, ID, ReadPerm, WritePerm, Value) ->
   Item = ataxia_entry:new(ID, ReadPerm, WritePerm, Value),
   case mnesia:transaction(fun add_new_item/2, [DB, Item]) of
      {'atomic', 'ok'} -> 'ok';
      {aborted, Val} -> {aborted, Val}
   end.

-spec insert
   (
      atom(),
      ataxia_security:permission(),
      ataxia_security:permission(),
      any())
   -> ({'aborted', any()} | {'ok', binary()}).
insert (DB, ReadPerm, WritePerm, Value) ->
   ID = db_item_ids_manager:allocate(DB),
   case insert_at(DB, ID, ReadPerm, WritePerm, Value) of
      {'atomic', 'ok'} -> {'ok', ID};
      {aborted, Val} -> {aborted, Val}
   end.

-spec query (shr_db_query:type()) -> ({'aborted', any()} | {'atomic', 'ok'}).
query (Query) ->
   mnesia:transaction(fun query_transaction/1, [Query]).

-spec reserve
   (
      atom(),
      binary(),
      ataxia_security:user()
   )
   -> ({'aborted', any()} | {'atomic', 'ok'}).
reserve (DB, ID, Cred) ->
   insert_at
   (
      DB,
      ID,
      [Cred],
      [Cred],
      {
         reserved,
         <<"?">> %% TODO [FUNCTION: db][LOW]: timestamp
      }
   ).

-spec remove
   (
      atom(),
      binary(),
      ataxia_security:user()
   )
   -> ({'aborted', any()} | 'ok' | 'not_found').
remove (_DB, _ID, _Cred) ->
   %% TODO [FUNCTION: db][MEDIUM]: unimplemented
   %% Don't forget to checkt that Cred has write access before removing the
   %% value.
   {'aborted', 'unimplemented'}.
