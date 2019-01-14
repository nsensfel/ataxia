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
      add_at/6,
      add/5,
      reserve/4,
      reserve_at/5,

      fetch/3,
      update/4,
      update_and_fetch/4,
      remove/3,

      fetch_if/4,
      update_if/5,
      update_and_fetch_if/5,
      remove_if/4,

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
   -> {'ok', ataxia_entry:type()}.
update_internals (DB, User, OP, ID) ->
   [Entry] = mnesia:read(DB, ID),

   true =
      (
         ataxia_lock:can_access(User, ataxia_entry:get_lock(Entry))
         and
         ataxia_security:can_access
         (
            User,
            ataxia_entry:get_write_permission(Entry)
         )
      ),

   UpdatedEntry = ataxic:apply_to(OP, Entry),

   mnesia:write(DB, UpdatedEntry, sticky_write),

   {ok, UpdatedEntry}.

-spec update_if
   (
      atom(),
      ataxia_security:user(),
      ataxic:meta(),
      ataxia_id:type(),
      ataxic:basic()
   )
   -> ({'ok', ataxia_entry:type()} | no_match).
update_if (DB, User, OP, ID, Cond) ->
   [Entry] = mnesia:read(DB, ID),

   true =
      (
         ataxia_lock:can_access(User, ataxia_entry:get_lock(Entry))
         and
         ataxia_security:can_access
         (
            User,
            ataxia_entry:get_write_permission(Entry)
         )
      ),

   case ataxic:matches(Cond, Entry) of
      true ->
         UpdatedEntry = ataxic:apply_to(OP, Entry),
         mnesia:write(DB, UpdatedEntry, sticky_write),
         {ok, UpdatedEntry};

      _ -> no_match
   end.

-spec remove_internals
   (
      atom(),
      ataxia_security:user(),
      ataxia_id:type()
   )
   -> 'ok'.
remove_internals (DB, User, ID) ->
   [Entry] = mnesia:read(DB, ID),

   true =
      (
         ataxia_lock:can_access(User, ataxia_entry:get_lock(Entry))
         and
         ataxia_security:can_access
         (
            User,
            ataxia_entry:get_write_permission(Entry)
         )
      ),

   mnesia:delete(DB, ID, sticky_write),
   ataxia_id_manager:free(ID, DB),

   ok.

-spec remove_if
   (
      atom(),
      ataxia_security:user(),
      ataxia_id:type(),
      ataxic:basic()
   )
   -> ('ok' | 'no_match').
remove_if (DB, User, ID, Cond) ->
   [Entry] = mnesia:read(DB, ID),

   true =
      (
         ataxia_lock:can_access(User, ataxia_entry:get_lock(Entry))
         and
         ataxia_security:can_access
         (
            User,
            ataxia_entry:get_write_permission(Entry)
         )
      ),

   case ataxic:matches(Cond, Entry) of
      true ->
         mnesia:delete(DB, ID, sticky_write),
         ataxia_id_manager:free(ID, DB),
         ok;

      _ -> no_match
   end.

-spec add_new_item (atom(), ataxia_entry:type()) -> 'ok'.
add_new_item (DB, Item) ->
   ID = ataxia_entry:get_id(Item),
   [] = mnesia:read(DB, ID),

   mnesia:write(DB, Item, sticky_write),

   ok.

-spec fetch_if
   (
      atom(),
      ataxia_security:user(),
      ataxia_id:type(),
      ataxic:basic()
   )
   -> ({'aborted', any()} | {'ok', any()}).
fetch_if (DB, User, ID, Cond) ->
   case mnesia:read(DB, ID) of
      [Entry] ->
         IsAllowed =
            (
               ataxia_lock:can_access(User, ataxia_entry:get_lock(Entry))
               and
               ataxia_security:can_access
               (
                  User,
                  ataxia_entry:get_read_permission(Entry)
               )
            ),
         case (IsAllowed and ataxic:matches(Cond, Entry)) of
            true -> {ok, ataxia_entry:get_value(Entry)};
            _ -> {aborted, no_match}
         end;

      Other -> Other
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec add_at
   (
      atom(),
      ataxia_id:type(),
      ataxia_security:permission(),
      ataxia_security:permission(),
      ataxia_lock:type(),
      any()
   )
   -> ({'aborted', any()} | 'ok').
add_at (DB, ID, ReadPerm, WritePerm, Lock, Value) ->
   Item = ataxia_entry:new(ID, ReadPerm, WritePerm, Lock, Value),
   case mnesia:transaction(fun add_new_item/2, [DB, Item]) of
      {atomic, ok} -> ok;
      {aborted, Val} -> {aborted, Val}
   end.

-spec add
   (
      atom(),
      ataxia_security:permission(),
      ataxia_security:permission(),
      ataxia_lock:type(),
      any()
   )
   -> ({'aborted', any()} | {'ok', ataxia_id:type()}).
add (DB, ReadPerm, WritePerm, Lock, Value) ->
   ID = ataxia_id_manager:allocate(DB),
   case add_at(DB, ID, ReadPerm, WritePerm, Lock, Value) of
      ok -> {ok, ID};
      {aborted, Val} -> {aborted, Val}
   end.

-spec reserve_at
   (
      atom(),
      ataxia_security:permission(),
      ataxia_security:permission(),
      ataxia_lock:type(),
      ataxia_id:type()
   )
   -> ({'aborted', any()} | 'ok').
reserve_at (DB, ReadPerm, WritePerm, Lock, ID) ->
   % TODO: spawn or inform janitor
   add_at
   (
      DB,
      ID,
      ataxia_security:add_access(ataxia_security:janitor(), ReadPerm),
      ataxia_security:add_access(ataxia_security:janitor(), WritePerm),
      Lock, % TODO: allow the janitor there.
      reserved
   ).

-spec reserve
   (
      atom(),
      ataxia_security:permission(),
      ataxia_security:permission(),
      ataxia_lock:type()
   )
   -> ({'aborted', any()} | {'ok', ataxia_id:type()}).
reserve (DB, ReadPerm, WritePerm, Lock) ->
   % TODO: spawn or inform janitor
   add
   (
      DB,
      ataxia_security:add_access(ataxia_security:janitor(), ReadPerm),
      ataxia_security:add_access(ataxia_security:janitor(), WritePerm),
      Lock, % TODO: allow the janitor there.
      reserved
   ).

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
         IsAllowed =
            (
               ataxia_lock:can_access(User, ataxia_entry:get_lock(Entry))
               and
               ataxia_security:can_access
               (
                  User,
                  ataxia_entry:get_read_permission(Entry)
               )
            ),
         case IsAllowed of
            true -> {ok, ataxia_entry:get_value(Entry)};
            false -> {aborted, permission_denied}
         end;

      Other -> {aborted, Other}
   end.

-spec fetch_any
   (
      atom(),
      ataxia_security:user(),
      ataxic:basic()
   )
   -> ({'aborted', any()} | {'ok', any(), ataxia_id:type()}).
fetch_any (DB, User, Cond) ->
   PotentialDBKeys = mnesia:dirty_all_keys(DB),
   Result =
      lists:foldl
      (
         fun (Key, Result) ->
            case Result of
               {ok, _, _} -> Result;
               _ ->
                  case
                     mnesia:transaction
                     (
                        fun fetch_if/4,
                        [DB, User, Key, Cond]
                     )
                  of
                     {atomic, {ok, Value}} -> {ok, Value, Key};
                     _ -> Result
                  end
            end
         end,
         {aborted, no_match},
         PotentialDBKeys
      ),

   Result.

-spec fetch_all
   (
      atom(),
      ataxia_security:user(),
      ataxic:basic()
   )
   -> ({'aborted', any()} | {'ok', list({any(), ataxia_id:type()})}).
fetch_all (DB, User, Cond) ->
   PotentialDBKeys = mnesia:dirty_all_keys(DB),
   Result =
      lists:filtermap
      (
         fun (Key) ->
            case
               mnesia:transaction
               (
                  fun fetch_if/4,
                  [DB, User, Key, Cond]
               )
            of
               {atomic, {ok, Value}} -> {true, {Value, Key}};
               _ -> false
            end
         end,
         PotentialDBKeys
      ),

   {ok, Result}.

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
      {atomic, {ok, _}} -> ok;
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
update_any (DB, User, Update, Cond) ->
   PotentialDBKeys = mnesia:dirty_all_keys(DB),
   Result =
      lists:foldl
      (
         fun (Key, Result) ->
            case Result of
               {ok, _} -> Result;
               _ ->
                  case
                     mnesia:transaction
                     (
                        fun update_if/5,
                        [DB, User, Update, Key, Cond]
                     )
                  of
                     {atomic, {ok, _}} -> {ok, Key};
                     _ -> Result
                  end
            end
         end,
         {aborted, no_match},
         PotentialDBKeys
      ),

   Result.

-spec update_all
   (
      atom(),
      ataxia_security:user(),
      ataxic:meta(),
      ataxic:basic()
   )
   -> ({'aborted', any()} | {'ok', list(ataxia_id:type())}).
update_all (DB, User, Update, Cond) ->
   PotentialDBKeys = mnesia:dirty_all_keys(DB),
   Result =
      lists:filtermap
      (
         fun (Key) ->
            case
               mnesia:transaction
               (
                  fun update_if/5,
                  [DB, User, Update, Key, Cond]
               )
            of
               {atomic, {ok, _}} -> {true, Key};
               _ -> false
            end
         end,
         PotentialDBKeys
      ),

   {ok, Result}.

-spec update_and_fetch
   (
      atom(),
      ataxia_security:user(),
      ataxic:meta(),
      ataxia_id:type()
   )
   -> ({'aborted', any()} | {'ok', any()}).
update_and_fetch (DB, User, Update, ID) ->
   case
      mnesia:transaction
      (
         fun update_internals/4,
         [DB, User, Update, ID]
      )
   of
      {atomic, {ok, Entry}} -> {ok, ataxia_entry:get_value(Entry)};
      Other -> {aborted, Other}
   end.

-spec update_and_fetch_any
   (
      atom(),
      ataxia_security:user(),
      ataxic:meta(),
      ataxic:basic()
   )
   -> ({'aborted', any()} | {'ok', any(), ataxia_id:type()}).
update_and_fetch_any (DB, User, Update, Cond) ->
   PotentialDBKeys = mnesia:dirty_all_keys(DB),
   Result =
      lists:foldl
      (
         fun (Key, Result) ->
            case Result of
               {ok, _} -> Result;
               _ ->
                  case
                     mnesia:transaction
                     (
                        fun update_if/5,
                        [DB, User, Update, Key, Cond]
                     )
                  of
                     {atomic, {ok, Entry}} ->
                        {ok, ataxia_entry:get_value(Entry), Key};

                     _ -> Result
                  end
            end
         end,
         {aborted, no_match},
         PotentialDBKeys
      ),

   Result.

-spec update_and_fetch_all
   (
      atom(),
      ataxia_security:user(),
      ataxic:meta(),
      ataxic:basic()
   )
   -> ({'aborted', any()} | {'ok', list({any(), ataxia_id:type()})}).
update_and_fetch_all (DB, User, Update, Cond) ->
   PotentialDBKeys = mnesia:dirty_all_keys(DB),
   Result =
      lists:filtermap
      (
         fun (Key) ->
            case
               mnesia:transaction
               (
                  fun update_if/5,
                  [DB, User, Update, Key, Cond]
               )
            of
               {atomic, {ok, Entry}} ->
                  {true, {ataxia_entry:get_value(Entry), Key}};

               _ -> false
            end
         end,
         PotentialDBKeys
      ),

   {ok, Result}.

-spec remove
   (
      atom(),
      ataxia_security:user(),
      ataxia_id:type()
   )
   -> ({'aborted', any()} | 'ok').
remove (DB, User, ID) ->
   case mnesia:transaction(fun remove_internals/3, [DB, User, ID]) of
      {atomic, ok} -> ok;
      Other -> {aborted, Other}
   end.

-spec remove_any
   (
      atom(),
      ataxia_security:user(),
      ataxic:basic()
   )
   -> ({'aborted', any()} | {'ok', ataxia_id:type()}).
remove_any (DB, User, Cond) ->
   PotentialDBKeys = mnesia:dirty_all_keys(DB),
   Result =
      lists:foldl
      (
         fun (Key, Result) ->
            case Result of
               {ok, _} -> Result;
               _ ->
                  case
                     mnesia:transaction
                     (
                        fun remove_if/4,
                        [DB, User, Key, Cond]
                     )
                  of
                     {atomic, ok} -> {ok, Key};
                     _ -> Result
                  end
            end
         end,
         {aborted, no_match},
         PotentialDBKeys
      ),

   Result.

-spec remove_all
   (
      atom(),
      ataxia_security:user(),
      ataxic:basic()
   )
   -> ({'aborted', any()} | {'ok', list(ataxia_id:type())}).
remove_all (DB, User, Cond) ->
   PotentialDBKeys = mnesia:dirty_all_keys(DB),
   Result =
      lists:filtermap
      (
         fun (Key) ->
            case
               mnesia:transaction
               (
                  fun remove_if/4,
                  [DB, User, Key, Cond]
               )
            of
               {atomic, ok} -> {true, Key};
               _ -> false
            end
         end,
         PotentialDBKeys
      ),

   {ok, Result}.
