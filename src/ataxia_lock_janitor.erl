-module(ataxia_lock_janitor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type lock_reference() :: {node(), pid()}.

-record
(
	state,
	{
		parent :: pid(),
		locks :: sets:set(lock_reference())
	}
).

-type type() :: pid().

-export_type([type/0, lock_reference/0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
	[
		new/0,
		request_lock/4,
		release_lock/2,
		release_all/1
	]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec release_lock (pid(), node(), pid()) -> ok.
release_lock (ParentPID, LockNode, LockPid) ->
	erpc:cast(LockNode, ataxia_lock, release_lock, [node(), ParentPID, LockPid]).

-spec janitor (#state{}) -> 'ok'.
janitor (State) ->
	receive
		{store, RemoteProcess} ->
			janitor
			(
				State#state
				{
					locks =
						set:add_element
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
					locks = set:del_element(RemoteProcess, State#state.locks)
				}
			);

		release_all ->
			set:fold
			(
				fun ({Node, Pid}, _Acc) ->
					release_lock(State#state.parent, Node, Pid)
				end,
				ok,
				State#state.locks
			),
			janitor(State#state{ locks = set:new() });

		{'EXIT', _Pid, _State} ->
			set:fold
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
		fun janitor/1,
		[#state{ parent = self(), locks = set:new() }]
	).

-spec request_lock
(
	atom(),
	ataxia_id:type(),
	('write' | 'read'),
	pid()
) -> ({ok, lock_reference()} | {error, any()}).
request_lock (DB, ID, Mode, Janitor) ->
	Result =
		ataxia_network:call
		(
			DB,
			ID,
			request_lock,
			[DB, ID, node(), self(), Mode]
		),
	case Result of
		{ok, LockReference} -> Janitor ! {store, LockReference};
		_Error -> ok
	end,
	Result.

-spec release_lock (lock_reference(), pid()) -> ok.
release_lock ({LockNode, LockPid}, Janitor) ->
	release_lock(self(), LockNode, LockPid),
	Janitor ! {remove, {LockNode, LockPid}},
	ok.

-spec release_all (pid()) -> ok.
release_all (Janitor) ->
	Janitor ! remove_all,
	ok.
