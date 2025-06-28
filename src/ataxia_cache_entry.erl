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
-record
(
	cache_entry,
	{
		value :: any(),
		version :: non_neg_integer()
	}
).

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

%%%% Interface
-export
(
	[
		start/0,
		request/3
	]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec reply_to (pid(), non_neg_integer(), term()) -> 'ok'.
reply_to (PID, RequestID, Reply) ->
	PID ! {ataxia_reply, RequestID, Reply},
	'ok'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 'gen_server' functions
init (_) -> {ok, none}.

% TODO: all "none" return should end this cache line and report to the cache
% manager.
% TODO: all non-"none" return should have a timeout to end this cache line and
% report to the cache manager.
% TODO: only use casts.
handle_cast ({request, ReplyTo, RequestID, {fetch, DB, ID}}, State) ->
	Request =
		case State of
			none -> [DB, ID, none];
			Entry -> [DB, ID, Entry#cache_entry.version]
		end,
	case ataxia_network:request(DB, ID, fetch, Request) of
		ok ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, State#cache_entry.version, State#cache_entry.value}
			),
			{noreply, State};

		{ok, {NewValue, NewVersion}} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, NewVersion, NewValue}
			),
			{
				noreply,
				#cache_entry{ value = NewValue, version = NewVersion }
			};

		Error ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{error, Error}
			),
			{stop, none, {DB, ID, self()}}
	end;
handle_cast
(
	{request, ReplyTo, RequestID, {blind_update, DB, ID, Op}},
	_State
) ->
	reply_to
	(
		ReplyTo,
		RequestID,
		case ataxia_network:request(DB, ID, blind_update, [DB, ID, Op]) of
			{ok, _} -> ok;
			Error -> {error, Error}
		end
	),
	{stop, none, {DB, ID, self()}};
handle_cast
(
	{
		request,
		ReplyTo,
		RequestID,
		{safe_update, DB, ID, Op, ExpectedCurrentVersion, ExpectedNewValue}
	},
	none
) ->
	% Cache may have been freed. Still do update attempt.
	Request = [DB, ID, ExpectedCurrentVersion, Op],
	case ataxia_network:request(DB, ID, safe_update, Request) of
		{ok, NewVersion} ->
			reply_to(ReplyTo, RequestID, {ok, NewVersion}),
			{
				noreply,
				#cache_entry
				{
					value = ExpectedNewValue,
					version = NewVersion
				}
			};

		Error ->
			reply_to(ReplyTo, RequestID, {error, Error}),
			{stop, none, {DB, ID, self()}}
	end;
handle_cast
(
	{
		request,
		ReplyTo,
		RequestID,
		{safe_update, DB, ID, _Op, ExpectedCurrentVersion, _ExpectedNewValue}
	},
	State
) when ExpectedCurrentVersion /= State#cache_entry.version ->
	reply_to(ReplyTo, RequestID, {error, version}),
	{stop, none, {DB, ID, self()}};
handle_cast
(
	{
		request,
		ReplyTo,
		RequestID,
		{safe_update, DB, ID, Op, ExpectedCurrentVersion, ExpectedNewValue}
	},
	_State
) ->
	Request = [DB, ID, ExpectedCurrentVersion, Op],
	case ataxia_network:request(DB, ID, safe_update, Request) of
		{ok, NewVersion} ->
			reply_to(ReplyTo, RequestID, {ok, NewVersion}),
			{
				noreply,
				#cache_entry
				{
					value = ExpectedNewValue,
					version = NewVersion
				}
			};

		Error ->
			reply_to(ReplyTo, RequestID, {error, Error}),
			{stop, none, {DB, ID, self()}}
	end;
handle_cast
(
	{
		request,
		ReplyTo,
		RequestID,
		{blind_update_then_fetch, DB, ID, Op}
	},
	_State
) ->
	case ataxia_network:request(DB, ID, blind_update_then_fetch, [DB, ID, Op]) of
		{ok, Version, Value} ->
			reply_to(ReplyTo, RequestID, {ok, Version, Value}),
			{
				noreply,
				#cache_entry
				{
					value = Value,
					version = Version
				}
			};

		Error ->
			reply_to(ReplyTo, RequestID, {error, Error}),
			{stop, none, {DB, ID, self()}}
	end;
handle_cast ({request, ReplyTo, RequestID, {blind_remove, DB, ID}}, _State) ->
	case ataxia_network:request(DB, ID, blind_remove, [DB, ID]) of
		ok -> reply_to(ReplyTo, RequestID, ok);
		Error -> reply_to(ReplyTo, RequestID, {error, Error})
	end,
	{stop, none, {DB, ID, self()}};
handle_cast
(
	{
		request,
		ReplyTo,
		RequestID,
		{safe_remove, DB, ID, ExpectedCurrentVersion}
	},
	none
) ->
	case
		ataxia_network:request
		(
			DB,
			ID,
			safe_remove,
			[DB, ID, ExpectedCurrentVersion]
		)
	of
		ok -> reply_to(ReplyTo, RequestID, ok);
		Error -> reply_to(ReplyTo, RequestID, {error, Error})
	end,
	{stop, none, {DB, ID, self()}};
handle_cast
(
	{
		request,
		ReplyTo,
		RequestID,
		{safe_remove, DB, ID, ExpectedCurrentVersion}
	},
	State
) when State#cache_entry.version /= ExpectedCurrentVersion ->
	reply_to(ReplyTo, RequestID, {error, version}),
	{stop, none, {DB, ID, self()}};
handle_cast
(
	{
		request,
		ReplyTo,
		RequestID,
		{safe_remove, DB, ID, ExpectedCurrentVersion}
	},
	_State
) ->
	case
		ataxia_network:request
		(
			DB,
			ID,
			safe_remove,
			[DB, ID, ExpectedCurrentVersion]
		)
	of
		ok -> reply_to(ReplyTo, RequestID, ok);
		Error -> reply_to(ReplyTo, RequestID, {error, Error})
	end,
	{stop, none, {DB, ID, self()}};
handle_cast
(
	{
		request,
		ReplyTo,
		RequestID,
		{fetch_if, DB, ID, Cond}
	},
	State
) ->
	Request =
		case State of
			none -> [DB, ID, none, Cond];
			Entry -> [DB, ID, Entry#cache_entry.version, Cond]
		end,
	case ataxia_network:request(DB, ID, fetch_if, Request) of
		ok ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, State#cache_entry.version, State#cache_entry.value}
			),
			{noreply, State};

		{ok, {NewValue, NewVersion}} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, NewVersion, NewValue}
			),
			{
				noreply,
				#cache_entry{ value = NewValue, version = NewVersion }
			};

		Error ->
			reply_to(ReplyTo, RequestID, {error, Error}),
			{stop, none, {DB, ID, self()}}
	end;
handle_cast
(
	{request, ReplyTo, RequestID, {blind_update_if, DB, ID, Cond, Op}},
	_State
) ->
	case ataxia_network:request(DB, ID, blind_update_if, [DB, ID, Cond, Op]) of
		{ok, _} -> reply_to(ReplyTo, RequestID, ok);
		Error -> reply_to(ReplyTo, RequestID, {error, Error})
	end,
	{stop, none, {DB, ID, self()}};
handle_cast
(
	{
		request,
		ReplyTo,
		RequestID,
		{blind_update_if_then_fetch, DB, ID, Cond, Op}
	},
	_State
) ->
	case
		ataxia_network:request
		(
			DB,
			ID,
			blind_update_if_then_fetch,
			[DB, ID, Cond, Op]
		)
	of
		{ok, Version, Value} ->
			reply_to(ReplyTo, RequestID, {ok, Version, Value}),
			{noreply, #cache_entry{ version = Version, value = Value }};

		Error ->
			reply_to(ReplyTo, RequestID, {error, Error}),
			{stop, none, {DB, ID, self()}}
	end;
handle_cast
(
	{
		request,
		ReplyTo,
		RequestID,
		{blind_update_if_else_fetch, DB, ID, Cond, Op}
	},
	_State
) ->
	case
		ataxia_network:request
		(
			DB,
			ID,
			blind_update_if_else_fetch,
			[DB, ID, Cond, Op]
		)
	of
		{ok, _} ->
			reply_to(ReplyTo, RequestID, ok),
			{stop, none, {DB, ID, self()}};

		{ok, Version, Value} ->
			reply_to(ReplyTo, RequestID, {ok, Version, Value}),
			{noreply, #cache_entry{ version = Version, value = Value }};

		Error ->
			reply_to(ReplyTo, RequestID, {error, Error}),
			{stop, none, {DB, ID, self()}}
	end;
handle_cast
(
	{request, ReplyTo, RequestID, {blind_remove_if, DB, ID, Cond}},
	State
) ->
	case ataxia_network:request(DB, ID, blind_remove_if, [DB, ID, Cond]) of
		ok ->
			reply_to(ReplyTo, RequestID, ok),
			{stop, none, {DB, ID, self()}};

		{error, condition} ->
			reply_to(ReplyTo, RequestID, {error, condition}),
			{noreply, State};

		Error ->
			reply_to(ReplyTo, RequestID, Error),
			{stop, none, {DB, ID, self()}}
	end.

handle_call (_Request, _From, State) ->
	{noreply, State}.

terminate (_, _) -> ok.

code_change (_, State, _) ->
	{ok, State}.

format_status (_, [_, State]) ->
	[{data, [{"State", State}]}].

handle_info(_, State) ->
	{noreply, State}.

%%%% Interface Functions
-spec start () -> {'ok', pid()}.
start () ->
	{ok, PID} = gen_server:start_link(?MODULE, none, []),
	{ok, PID}.

-spec request (pid(), non_neg_integer(), term()) -> 'ok'.
request (EntryPID, RequestID, Request) ->
	gen_server:cast(EntryPID, {request, self(), RequestID, Request}).
