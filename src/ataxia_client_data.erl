%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ATAXIA CLIENT DATA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ataxia_client_data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
	data,
	{
		db :: atom(),
		id :: ataxia_id:type(),
		value :: any(),
		version :: non_neg_integer(),
		updates :: list(ataxic:type()),
		lock :: ataxia_network:proc()
	}
).

-type type(DataType) :: #data{value :: DataType}.
-type type() :: type(any()).

-export_type([type/0, type/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
	[
		new/5,

		add_update/3,
		update_to/4,
		set_lock/2,

		get_database/1,
		get_id/1,
		get_lock/1,
		get_ataxic/1,
		get_value/1
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
		atom(),
		ataxia_id:type(),
		ataxia_network:proc(),
		non_neg_integer(),
		A
	)
	-> type(A).
new (DB, ID, Lock, Version, Value) ->
	#data
	{
		db = DB,
		id = ID,
		value = Value,
		version = Version,
		updates = [],
		lock = Lock
	}.

-spec add_update (ataxic:type(), A, type(A)) -> type(A).
add_update (Update, NewValue, ClientData) ->
	ClientData#data
	{
		updates = [Update|ClientData#data.updates],
		value = NewValue
	}.

-spec update_to
	(
		ataxia_network:proc(),
		non_neg_integer(),
		A,
		type(A)
	)
	-> type(A).
update_to (NewLock, NewVersion, NewValue, ClientData) ->
	ClientData#data
	{
		lock = NewLock,
		version = NewVersion,
		value = NewValue
	}.

-spec set_lock (ataxia_network:proc(), type(A)) -> type(A).
set_lock (NewLock, ClientData) -> ClientData#data{ lock = NewLock }.

-spec get_database (type(_)) -> atom().
get_database (#data{ db = Result }) -> Result.

-spec get_id (type(_)) -> ataxia_id:type().
get_id (#data{ id = Result }) -> Result.

-spec get_lock (type(_)) -> ataxia_network:proc().
get_lock (#data{ lock = Result }) -> Result.

-spec get_value (type(A)) -> A.
get_value (#data{ value = Result }) -> Result.

-spec get_ataxic (type(_)) -> ataxic:type().
get_ataxic (#data{ updates = Updates }) ->
	ataxic:sequence(lists:reverse(Updates)).
