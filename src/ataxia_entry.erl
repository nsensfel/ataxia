-module(ataxia_entry).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   entry,
   {
      id :: any(),
      read_perm :: ataxia_security:permission(),
      write_perm :: ataxia_security:permission(),
      lock :: ataxia_lock:type(),
      val :: any()
   }
).

-type type() :: #entry{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([type/0]).

-export
(
   [
      new/5,

      get_id/1,
      get_read_permission/1,
      get_write_permission/1,
      get_value/1,
      get_lock/1,

      set_read_permission/2,
      set_write_permission/2,
      set_value/2,
      set_lock/2,

      get_id_field/0,
      get_value_field/0,
      get_read_permission_field/0,
      get_write_permission_field/0,
      get_lock_field/0,

      get_record_info/0,
      get_record_name/0
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new
   (
      any(),
      ataxia_security:permission(),
      ataxia_security:permission(),
      ataxia_lock:type(),
      any()
   ) -> type().
new (ID, ReadPermission, WritePermission, Lock, Value) ->
   #entry
   {
      id = ID,
      read_perm = ReadPermission,
      write_perm = WritePermission,
      lock = Lock,
      val = Value
   }.

-spec get_id (type()) -> any().
get_id (#entry { id = Result }) -> Result.

-spec get_read_permission (type()) -> ataxia_security:permission().
get_read_permission (#entry { read_perm = Result }) -> Result.

-spec get_write_permission (type()) -> ataxia_security:permission().
get_write_permission (#entry { write_perm = Result }) -> Result.

-spec get_value (type()) -> any().
get_value (#entry { val = Result }) -> Result.

-spec get_lock (type()) -> any().
get_lock (#entry { lock = Result }) -> Result.

-spec set_read_permission (ataxia_security:permission(), type()) -> type().
set_read_permission (Perm, Item) -> Item#entry{ read_perm = Perm }.

-spec set_write_permission (ataxia_security:permission(), type()) -> type().
set_write_permission (Perm, Item) -> Item#entry{ write_perm = Perm }.

-spec set_value (any(), type()) -> type().
set_value (Value, Item) -> Item#entry{ val = Value }.

-spec set_lock (ataxia_lock:type(), type()) -> type().
set_lock (Lock, Item) -> Item#entry{ lock = Lock }.

-spec get_id_field () -> non_neg_integer().
get_id_field () -> #entry.id.

-spec get_value_field () -> non_neg_integer().
get_value_field () -> #entry.val.

-spec get_read_permission_field () -> non_neg_integer().
get_read_permission_field () -> #entry.read_perm.

-spec get_write_permission_field () -> non_neg_integer().
get_write_permission_field () -> #entry.write_perm.

-spec get_lock_field () -> non_neg_integer().
get_lock_field () -> #entry.lock.

get_record_info () -> record_info(fields, entry).

get_record_name () -> entry.

