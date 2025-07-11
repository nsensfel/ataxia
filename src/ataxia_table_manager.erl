-module(ataxia_table_manager).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
	manager,
	{
		last_id :: ataxia_id:type(),
		free_ids :: list(ataxia_id:type())
	}
).

-type type() :: #manager{} | free.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Actual Interface
-export
(
	[
		new/0,
		none/0,
		get_new_id/1,
		release_id/2
	]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new() -> type().
new() ->
	#manager
	{
		last_id = ataxia_id:table_manager(),
		free_ids = []
	}.

-spec none () -> type().
none () -> free.

-spec get_new_id (type()) -> {ataxia_id:type(), type()}.
get_new_id (free) -> {<<"">>, free};
get_new_id (Manager) ->
	case Manager#manager.free_ids of
		[A|B] -> {A, Manager#manager{ free_ids = B } };

		[] ->
			NextID = ataxia_id:next(Manager#manager.last_id),
			{
				NextID,
				Manager#manager{ last_id = NextID }
			}
	end.

-spec release_id (ataxia_id:type(), type()) -> type().
release_id (_ID, free) -> free;
release_id (ID, Manager) ->
	Manager#manager
	{
		free_ids = [ID | Manager#manager.free_ids]
	}.
