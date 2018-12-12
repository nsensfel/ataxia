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
      lock :: ataxia_security:lock(),
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
      new/4,

      get_id/1,
      get_read_permission/1,
      get_write_permission/1,
      get_value/1,

      set_read_permission/2,
      set_write_permission/2,
      set_value/2,

      get_id_field/0,
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
      any()
   ) -> type().
new (ID, ReadPermission, WritePermission, Value) ->
   #entry
   {
      id = ID,
      read_perm = ReadPermission,
      write_perm = WritePermission,
      lock = ataxia_security:unlocked(),
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

-spec set_read_permission (ataxia_security:permission(), type()) -> type().
set_read_permission (Perm, Item) -> Item#entry{ read_perm = Perm }.

-spec set_write_permission (ataxia_security:permission(), type()) -> type().
set_write_permission (Perm, Item) -> Item#entry{ write_perm = Perm }.

-spec set_value (any(), type()) -> type().
set_value (Value, Item) -> Item#entry{ val = Value }.

-spec get_id_field () -> non_neg_integer().
get_id_field () -> #entry.id.

get_record_info () -> record_info(fields, entry).

get_record_name () -> entry.

