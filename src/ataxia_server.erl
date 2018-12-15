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
      add_at/5,
      add/4,
      reserve/2,

      fetch/3,
      update/4,
      update_and_fetch/4,
      remove/3,

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

-spec update_internals
   (
      atom(),
      ataxia_security:user(),
      ataxic:meta(),
      ataxia_id:type()
   )
   -> 'ok'.
update_internals (DB, User, OP, ID) ->
   [Entry] = mnesia:read(DB, ID),

   true =
      ataxia_security:can_access
      (
         ataxia_entry:get_write_permission(Entry),
         User
      ),

   mnesia:write(DB, ataxic:apply_to(OP, Entry), sticky_write),

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
-spec add_at
   (
      atom(),
      ataxia_id:type(),
      ataxia_security:permission(),
      ataxia_security:permission(),
      any()
   )
   -> ({'aborted', any()} | 'ok').
add_at (DB, ID, ReadPerm, WritePerm, Value) ->
   Item = ataxia_entry:new(ID, ReadPerm, WritePerm, Value),
   case mnesia:transaction(fun add_new_item/2, [DB, Item]) of
      {atomic, ok} -> ok;
      {aborted, Val} -> {aborted, Val}
   end.

-spec add
   (
      atom(),
      ataxia_security:permission(),
      ataxia_security:permission(),
      any()
   )
   -> ({'aborted', any()} | {'ok', ataxia_id:type()}).
add (DB, ReadPerm, WritePerm, Value) ->
   ID = ataxia_id_manager:allocate(DB),
   case add_at(DB, ID, ReadPerm, WritePerm, Value) of
      ok -> {ok, ID};
      {aborted, Val} -> {aborted, Val}
   end.

-spec reserve
   (
      atom(),
      ataxia_id:type()
   )
   -> ({'aborted', any()} | 'ok').
reserve (DB, ID) ->
   JanitorPermission = ataxia_security:allow_only(ataxia_security:janitor()),
   add_at(DB, ID, JanitorPermission, JanitorPermission, reserved).

-spec fetch
   (
      atom(),
      ataxia_security:user(),
      ataxia_id:type()
   )
   -> ({'aborted', any()} | {'ok', any()}).
fetch (DB, User, ID) ->
   case mnesia:transaction(fun mnesia:read/2, [DB, ID]) of
      {atomic, []} -> {aborted, not_found};
      {atomic, [Entry]} ->
         true =
            (
               ataxia_lock:can_access(User, ataxia_entry:get_lock(Entry))
               and
               ataxia_security:can_access
               (
                  ataxia_entry:get_read_permission(Entry),
                  User
               )
            ),
         {ok, ataxia_entry:get_value(Entry)};

      Other -> {aborted, Other}
   end.

-spec fetch_any
   (
      atom(),
      ataxia_security:user(),
      ataxic:basic()
   )
   -> ({'aborted', any()} | {'ok', any(), ataxia_id:type()}).
fetch_any (_DB, _User, _Cond) ->
   {aborted, unimplemented}.

-spec fetch_all
   (
      atom(),
      ataxia_security:user(),
      ataxic:basic()
   )
   -> ({'aborted', any()} | {'ok', any(), ataxia_id:type()}).
fetch_all (_DB, _User, _Cond) ->
   {aborted, unimplemented}.

-spec update
   (
      atom(),
      ataxia_security:user(),
      ataxic:meta(),
      ataxia_id:type()
   )
   -> ({'aborted', any()} | 'ok').
update (DB, User, Update, ID) ->
   case
      mnesia:transaction
      (
         fun update_internals/4,
         [DB, User, Update, ID]
      )
   of
      {atomic, ok} -> ok;
      Other -> {aborted, Other}
   end.

-spec update_any
   (
      atom(),
      ataxia_security:user(),
      ataxic:meta(),
      ataxic:basic()
   )
   -> ({'aborted', any()} | {'ok', ataxia_id:type()}).
update_any (_DB, _User, _Update, _Cond) ->
   {aborted, unimplemented}.

-spec update_all
   (
      atom(),
      ataxia_security:user(),
      ataxic:meta(),
      ataxic:basic()
   )
   -> ({'aborted', any()} | {'ok', list(ataxia_id:type())}).
update_all (_DB, _User, _Update, _Cond) ->
   {aborted, unimplemented}.

-spec update_and_fetch
   (
      atom(),
      ataxia_security:user(),
      ataxic:meta(),
      ataxia_id:type()
   )
   -> ({'aborted', any()} | {'ok', any()}).
update_and_fetch (_DB, _User, _Update, _ID) ->
   {aborted, unimplemented}.

-spec update_and_fetch_any
   (
      atom(),
      ataxia_security:user(),
      ataxic:meta(),
      ataxic:basic()
   )
   -> ({'aborted', any()} | {'ok', any(), ataxia_id:type()}).
update_and_fetch_any (_DB, _User, _Update, _Cond) ->
   {aborted, unimplemented}.

-spec update_and_fetch_all
   (
      atom(),
      ataxia_security:user(),
      ataxic:meta(),
      ataxic:basic()
   )
   -> ({'aborted', any()} | {'ok', list({any(), ataxia_id:type()})}).
update_and_fetch_all (_DB, _User, _Update, _Cond) ->
   {aborted, unimplemented}.

-spec remove
   (
      atom(),
      ataxia_security:user(),
      ataxia_id:type()
   )
   -> ({'aborted', any()} | 'ok').
remove (_DB, _User, _ID) ->
   {aborted, unimplemented}.

-spec remove_any
   (
      atom(),
      ataxia_security:user(),
      ataxic:basic()
   )
   -> ({'aborted', any()} | {'ok', ataxia_id:type()}).
remove_any (_DB, _User, _Cond) ->
   {aborted, unimplemented}.

-spec remove_all
   (
      atom(),
      ataxia_security:user(),
      ataxic:basic()
   )
   -> ({'aborted', any()} | {'ok', list(ataxia_id:type())}).
remove_all (_DB, _User, _Cond) ->
   {aborted, unimplemented}.
