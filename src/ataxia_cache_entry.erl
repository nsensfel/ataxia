%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ATAXIA CACHED ENTRY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ataxia entries cached on the client, to reduce network usage.
%
% This module defines the process for a cached DB entry. All requests for the
% entry are handled by this process, which allows serialization on the node.
% These processes should only be initiated through the ataxia cache module.
%
-module(ataxia_cache_entry).
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
		handle_call/3, %% No reply will ever be given.
		terminate/2,
		code_change/3,
		format_status/2,
		handle_info/2
	]
).

-export
(
	[
		read_lock/1,
		write_lock/1,
		read/1, % This also pings to delay the timeout
	]
).

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
	gen_server:call({global, ataxia_cache}, {request_cache_entry, DB, ID}).

-spec start () -> 'ok'.
start () ->
	{ok, _} = gen_server:start({global, ataxia_cache}, ?MODULE, none, []),

	ok.
