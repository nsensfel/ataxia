-module(ataxia_lock_janitor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
	state,
	{
		parent :: pid(),
		locks :: sets:set(ataxia_network:proc())
	}
).

-type type() :: pid().

-export_type([type/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
	[
		new/0,
		request_lock/4,
		release_lock/2,
		release_all/1,
		janitor/1
	]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec release_lock (pid(), node(), pid()) -> ok.
release_lock (ParentPID, LockNode, LockPid) ->
	erpc:cast(LockNode, ataxia_lock, release_lock, [LockPid, {node(), ParentPID}]).

-spec janitor (#state{}) -> 'ok'.
janitor (State) ->
	receive
		{store, RemoteProcess} ->
			janitor
			(
				State#state
				{
					locks =
						sets:add_element
						(
							RemoteProcess,
							State#state.locks
						)
				}
			);

		{remove, RemoteProcess} ->
			janitor
			(
				State#state
				{
					locks = sets:del_element(RemoteProcess, State#state.locks)
				}
			);

		release_all ->
			sets:fold
			(
				fun ({Node, Pid}, _Acc) ->
					release_lock(State#state.parent, Node, Pid)
				end,
				ok,
				State#state.locks
			),
			janitor(State#state{ locks = sets:new() });

		{'EXIT', _Pid, _State} ->
			sets:fold
			(
				fun ({Node, Pid}, _Acc) ->
					release_lock(State#state.parent, Node, Pid)
				end,
				ok,
				State#state.locks
			),
			ok
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new () -> pid().
new () ->
	spawn_link
	(
		?MODULE,
		janitor,
		[#state{ parent = self(), locks = sets:new() }]
	).

-spec request_lock
(
	atom(),
	ataxia_id:type(),
	('write' | 'read'),
	pid()
) -> ({ok, ataxia_network:proc()} | {error, any()}).
request_lock (DB, ID, Mode, Janitor) ->
	Result =
		ataxia_network:call
		(
			DB,
			ID,
			ataxia_lock,
			request_lock,
			[DB, ID, node(), self(), Mode]
		),
	case Result of
		{ok, LockReference} -> Janitor ! {store, LockReference};
		_Error -> ok
	end,
	Result.

-spec release_lock (ataxia_network:proc(), pid()) -> ok.
release_lock ({LockNode, LockPid}, Janitor) ->
	release_lock(self(), LockNode, LockPid),
	Janitor ! {remove, {LockNode, LockPid}},
	ok.

-spec release_all (pid()) -> ok.
release_all (Janitor) ->
	Janitor ! remove_all,
	ok.
