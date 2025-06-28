%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ATAXIA CACHE MANAGER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Manager for cached entry processes.
%
-module(ataxia_cache_manager).
-behavior(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
		request_cache_entry/2,
		start/0
	]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% FIXME: tie cache entries to manager, so it gets notified of termination.
%%%% FIXME: multiple cache managers according to ID modulo?
-spec request_cache_entry (atom(), ataxia_id:type(), ets:tab()) ->
	{
		pid(),
		ets:tab()
	}.
request_cache_entry (DB, ID, State) ->
	case ets:lookup(State, {DB, ID}) of
		[{_, Result}] -> {Result, State};
		[] ->
			Result = ataxia_cache_entry:start(DB, ID),
			NewState = ets:insert(State, {{DB, ID}, Result}),
			{Result, NewState}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TODO: Client object for all client data (known cache lines, cache line manager
%%%% references - fed at init).
%%%% 'gen_server' functions
init (_) -> {ok, ets:new(cache_entries, [])}.

handle_call ({request_cache_entry, DB, ID}, _From, State) ->
	{PID, NewState} = request_cache_entry(DB, ID, State),
	{reply, PID, NewState};
handle_call ({free, ID, DB}, _, State) ->
	{noreply, free_id(ID, DB, State)}.

handle_cast ({request_cache_entry, DB, ID}, State) ->
	{PID, NewState} = request_cache_entry(DB, ID, State),
	{reply, PID, NewState}.

terminate (_, _) -> ok.

code_change (_, State, _) ->
	{ok, State}.

format_status (_, [_, State]) ->
	[{data, [{"State", State}]}].

handle_info(_, State) ->
	{noreply, State}.

%%%% Interface Functions
-spec request_cache_entry (atom(), ataxia_id:type()) -> pid().
request_cache_entry (DB, ID) ->
	gen_server:call
	(
		{global, {ataxia_cache_manager, DB}},
		{request_cache_entry, DB, ID}
	).

-spec start (DB) -> 'ok'.
start (DB) ->
	{ok, _} =
		gen_server:start({global, {ataxia_cache_manager, DB}}, ?MODULE, none, []),

	ok.
