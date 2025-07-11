%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ATAXIA CACHE MANAGER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Manager for cached entry processes.
%
-module(ataxia_cache_manager).
-behavior(gen_server).

-define(MANAGER_COUNT, 12).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type state() :: #{{atom(), ataxia_id:type()} := pid()}.
-type collection() :: array:array(atom()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 'gen_server' Exports
-export
(
	[
		init/1,
		handle_cast/2,
		handle_call/3,
		terminate/2,
		code_change/3,
		format_status/2,
		handle_info/2
	]
).

-export
(
	[
		request_entry_for/4,
		peek_entry_for/3,
		start/1,
		start_collection/0,
		get_collection/0
	]
).

-export_type([collection/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec request_entry (atom(), ataxia_id:type(), state()) ->
	{
		pid(),
		state()
	}.
request_entry (DB, ID, State) ->
	case maps:find({DB, ID}, State) of
		{ok, Result} -> {Result, State};
		error ->
			Result = ataxia_cache_entry:start(),
			NewState = maps:put({DB, ID}, Result, State),
			{Result, NewState}
	end.

-spec generate_atom_from_index (non_neg_integer()) -> atom().
generate_atom_from_index (IX) ->
	list_to_atom
	(
		lists:flatten
		(
			io_lib:format
			(
				"ataxia_cache_manager_~p",
				[IX]
			)
		)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init (_) ->
	process_flag(trap_exit, true),
	{ok, maps:new()}.

handle_call ({request_entry, DB, ID, none}, _From, State) ->
	{PID, NewState} = request_entry(DB, ID, State),
	{reply, PID, NewState};
handle_call ({request_entry, DB, ID, CurrentPID}, _From, State) ->
	{PID, NewState} =
		case maps:find({DB, ID}, State) of
			{ok, OtherValue} when OtherValue /= CurrentPID -> {OtherValue, State};
			_ -> request_entry(DB, ID, maps:remove({DB, ID}, State))
		end,

	{reply, PID, NewState};
handle_call ({peek_entry, DB, ID}, _From, State) ->
	PID =
		case maps:find({DB, ID}, State) of
			{ok, Value} -> Value;
			_ -> none
		end,

	{reply, PID, State}.

handle_cast (_Msg, State) -> {noreply, State}.

terminate (_, _) -> ok.

code_change (_, State, _) ->
	{ok, State}.

format_status (_, [_, State]) ->
	[{data, [{"State", State}]}].

handle_info({'EXIT', _From, Reason}, State) ->
	case Reason of
		{shutdown, {DB, ID}} ->
			erlang:display({"Cache Manager removing entry", DB, ID}),
			{noreply, maps:remove({DB, ID}, State)};

		_ -> {noreply, State}
	end;
handle_info(What, State) ->
	erlang:display({"Cache Manager got other msg:", What, State}),
	{noreply, State}.

%%%% Interface Functions
-spec request_entry_for
	(
		collection(),
		atom(),
		ataxia_id:type(),
		(pid() | 'none')
	)
	-> pid().
request_entry_for (Collection, DB, ID, CurrentPID) ->
	ManagerAtom = array:get(ataxia_id:mod(ID, ?MANAGER_COUNT), Collection),
	gen_server:call(ManagerAtom, {request_entry, DB, ID, CurrentPID}).

-spec start (non_neg_integer()) -> 'ok'.
start (IX) ->
	{ok, _} =
		gen_server:start
		(
			{local, generate_atom_from_index(IX)},
			?MODULE,
			none,
			[]
		),

	ok.

-spec start_collection () -> 'ok'.
start_collection () ->
	lists:map
	(
		fun start/1,
		lists:seq(0, ?MANAGER_COUNT - 1)
	),
	ok.

-spec peek_entry_for
	(
		collection(),
		atom(),
		ataxia_id:type()
	)
	-> (pid() | 'none').
peek_entry_for (Collection, DB, ID) ->
	ManagerAtom = array:get(ataxia_id:mod(ID, ?MANAGER_COUNT), Collection),
	gen_server:call(ManagerAtom, {peek_entry, DB, ID}).

-spec get_collection () -> collection().
get_collection () ->
	array:from_list
	(
		lists:map
		(
			fun generate_atom_from_index/1,
			lists:seq(0, ?MANAGER_COUNT - 1)
		)
	).
