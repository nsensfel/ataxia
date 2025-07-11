-module(ataxia_table_manager).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
	manager,
	{
		last_id :: ataxia_id:type(),
		last_counter_id :: ataxia_id:type(),
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
		get_last_id/1,
		generate_id/1,
		release_id/2,
		ataxic_generate_id/0
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
		last_counter_id = ataxia_id:table_manager(),
		last_id = ataxia_id:table_manager(),
		free_ids = []
	}.

-spec get_last_id (type()) -> ataxia_id:type().
get_last_id (free) -> <<"">>;
get_last_id (#manager{ last_id = Result }) -> Result.

-spec none () -> type().
none () -> free.

-spec generate_id (type()) -> type().
generate_id (free) -> free;
generate_id (Manager) ->
	case Manager#manager.free_ids of
		[A|B] -> Manager#manager{ free_ids = B, last_id = A};

		[] ->
			NextID = ataxia_id:next(Manager#manager.last_id),
			Manager#manager{ last_counter_id = NextID, last_id = NextID }
	end.

-spec ataxic_generate_id () -> ataxic:type().
ataxic_generate_id () ->
	ataxic:apply_function
	(
		?MODULE,
		generate_id,
		[ataxic:current_value()]
	).

-spec release_id (ataxia_id:type(), type()) -> type().
release_id (_ID, free) -> free;
release_id (ID, Manager) ->
	Manager#manager
	{
		free_ids = [ID | Manager#manager.free_ids]
	}.
