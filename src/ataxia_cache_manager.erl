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
-type state() :: dict:dict({atom(), ataxia_id:type()}, pid()).

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
-spec request_cache_entry (atom(), ataxia_id:type(), state()) ->
	{
		pid(),
		state()
	}.
request_cache_entry (DB, ID, State) ->
	case dict:find({DB, ID}, State) of
		{ok, Result} -> {Result, State};
		error ->
			Result = ataxia_cache_entry:start(DB, ID),
			NewState = dict:store({DB, ID}, Result, State),
			{Result, NewState}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TODO: Client object for all client data (known cache lines, cache line manager
%%%% references - fed at init).
%%%% 'gen_server' functions
init (_) -> {ok, dict:new()}.

handle_call ({request_cache_entry, DB, ID}, _From, State) ->
	{PID, NewState} = request_cache_entry(DB, ID, State),
	{reply, PID, NewState}.

handle_cast ({request_cache_entry, DB, ID}, State) ->
	{PID, NewState} = request_cache_entry(DB, ID, State),
	{reply, PID, NewState}.

terminate (_, _) -> ok.

code_change (_, State, _) ->
	{ok, State}.

format_status (_, [_, State]) ->
	[{data, [{"State", State}]}].

handle_info({'EXIT', _From, {none, {DB, ID, _PID}}}, State) ->
	{noreply, dict:erase({DB, ID}, State)}.

%%%% Interface Functions
-spec request_cache_entry (atom(), ataxia_id:type()) -> pid().
request_cache_entry (DB, ID) ->
	IX = ataxia_id:mod(ID, ?MANAGER_COUNT),
	gen_server:call
	(
		{global, {ataxia_cache_manager, IX}},
		{request_cache_entry, DB, ID}
	).

-spec start (non_neg_integer()) -> 'ok'.
start (IX) ->
	{ok, _} =
		gen_server:start({global, {ataxia_cache_manager, IX}}, ?MODULE, none, []),

	ok.
