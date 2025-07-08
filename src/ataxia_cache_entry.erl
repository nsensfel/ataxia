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
handle_cast
(
	{request, ReplyTo, RequestID, {add, DB, Lock, Value}},
	_State
) ->
	case
		ataxia_network:call
		(
			DB,
			ataxia_id:table_manager(),
			ataxia_server,
			add,
			[DB, Lock, Value])
	of
		{ok, ID} -> {noreply, State};
		{ok, ID, NewLock} -> {noreply, State};
		Error -> {error, Error}
	end
	{stop, none, {DB, ID, self()}};
handle_cast
(
	{request, ReplyTo, RequestID, {add_at, DB, ID, Lock, Value}},
	_State
) ->
	case
		ataxia_network:call
		(
			DB,
			ataxia_id:table_manager(),
			ataxia_server,
			add_at,
			[DB, ID, Lock, Value])
	of
		ok -> {noreply, State};
		{stop, none, {DB, ID, self()}};
		Error -> {error, Error}
	end
handle_cast
(
	{request, ReplyTo, RequestID, {fetch, DB, ID, Lock}},
	State
) ->
	Request =
		case State of
			none -> [DB, ID, Lock, none];
			Entry -> [DB, ID, Lock, Entry#cache_entry.version]
		end,
	case ataxia_network:call(DB, ID, ataxia_server, fetch, Request) of
		ok ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, State#cache_entry.version, State#cache_entry.value}
			),
			{noreply, State};

		{ok, NewLock} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, NewLock, State#cache_entry.version, State#cache_entry.value}
			),
			{noreply, State};

		{ok, NewVersion, NewValue} ->
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

		{ok, NewLock, NewVersion, NewValue} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, NewLock, NewVersion, NewValue}
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
	{request, ReplyTo, RequestID, {blind_update, DB, ID, Lock, Op}},
	_State
) ->
	reply_to
	(
		ReplyTo,
		RequestID,
		case
			ataxia_network:call
			(
				DB,
				ID,
				ataxia_server,
				blind_update,
				[DB, ID, Lock, Op])
		of
			{ok, NewLock, NewVersion} -> {ok, NewLock, NewVersion};
			{ok, NewVersion} -> {ok, NewVersion};
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
		{safe_update, DB, ID, Lock, Op, ExpectedCurrentVersion, ExpectedNewValue}
	},
	none
) ->
	% Cache may have been freed. Still do update attempt.
	Request = [DB, ID, Lock, ExpectedCurrentVersion, Op],
	case ataxia_network:call(DB, ID, ataxia_server, safe_update, Request) of
		{ok, NewLock, NewVersion} ->
			reply_to(ReplyTo, RequestID, {ok, NewLock, NewVersion}),
			{
				noreply,
				#cache_entry
				{
					value = ExpectedNewValue,
					version = NewVersion
				}
			};

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
		{
			safe_update,
			DB,
			ID,
			_Lock,
			_Op,
			ExpectedCurrentVersion,
			_ExpectedNewValue
		}
	},
	State
) when ExpectedCurrentVersion /= State#cache_entry.version ->
	% Is this a good idea? Any case where it would be reasonable for
	% the cache to have the wrong version number and the code the correct one?
	reply_to(ReplyTo, RequestID, {error, version}),
	{stop, none, {DB, ID, self()}};
handle_cast
(
	{
		request,
		ReplyTo,
		RequestID,
		{safe_update, DB, ID, Lock, Op, ExpectedCurrentVersion, ExpectedNewValue}
	},
	_State
) ->
	Request = [DB, ID, Lock, ExpectedCurrentVersion, Op],
	case ataxia_network:call(DB, ID, ataxia_server, safe_update, Request) of
		{ok, NewLock, NewVersion} ->
			reply_to(ReplyTo, RequestID, {ok, NewLock, NewVersion}),
			{
				noreply,
				#cache_entry
				{
					value = ExpectedNewValue,
					version = NewVersion
				}
			};

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
		{blind_update_then_fetch, DB, ID, Lock, Op}
	},
	_State
) ->
	case
		ataxia_network:call
		(
			DB,
			ID,
			ataxia_server,
			blind_update_then_fetch,
			[DB, ID, Lock, Op]
		)
	of
		{ok, NewLock, Version, Value} ->
			reply_to(ReplyTo, RequestID, {ok, NewLock, Version, Value}),
			{
				noreply,
				#cache_entry
				{
					value = Value,
					version = Version
				}
			};

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
handle_cast
(
	{request, ReplyTo, RequestID, {blind_remove, DB, ID, Lock}},
	_State
) ->
	case
		ataxia_network:call(DB, ID, ataxia_server, blind_remove, [DB, ID, Lock])
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
		{safe_remove, DB, ID, Lock, ExpectedCurrentVersion}
	},
	none
) ->
	case
		ataxia_network:call
		(
			DB,
			ID,
			ataxia_server,
			safe_remove,
			[DB, ID, Lock, ExpectedCurrentVersion]
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
		{safe_remove, DB, ID, _Lock, ExpectedCurrentVersion}
	},
	State
) when State#cache_entry.version /= ExpectedCurrentVersion ->
	% Is this a good idea? Any case where it would be reasonable for
	% the cache to have the wrong version number and the code the correct one?
	reply_to(ReplyTo, RequestID, {error, version}),
	{stop, none, {DB, ID, self()}};
handle_cast
(
	{
		request,
		ReplyTo,
		RequestID,
		{safe_remove, DB, ID, Lock, ExpectedCurrentVersion}
	},
	_State
) ->
	case
		ataxia_network:call
		(
			DB,
			ID,
			ataxia_server,
			safe_remove,
			[DB, ID, Lock, ExpectedCurrentVersion]
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
		{fetch_if, DB, ID, Lock, Cond}
	},
	State
) ->
	Request =
		case State of
			none -> [DB, ID, Lock, none, Cond];
			Entry -> [DB, ID, Lock, Entry#cache_entry.version, Cond]
		end,
	case ataxia_network:call(DB, ID, ataxia_server, fetch_if, Request) of
		ok ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, State#cache_entry.version, State#cache_entry.value}
			),
			{noreply, State};

		{ok, NewLock} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, NewLock, State#cache_entry.version, State#cache_entry.value}
			),
			{noreply, State};

		{ok, NewValue, NewVersion} ->
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

		{ok, NewLock, NewValue, NewVersion} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, NewLock, NewVersion, NewValue}
			),
			{
				noreply,
				#cache_entry{ value = NewValue, version = NewVersion }
			};

		{error, condition} ->
			reply_to(ReplyTo, RequestID, {error, condition}),
			{noreply, State};

		Error ->
			reply_to(ReplyTo, RequestID, {error, Error}),
			{stop, none, {DB, ID, self()}}
	end;
handle_cast
(
	{request, ReplyTo, RequestID, {blind_update_if, DB, ID, Lock, Cond, Op}},
	_State
) ->
	case
		ataxia_network:call
		(
			DB,
			ID,
			ataxia_server,
			blind_update_if, [DB, ID, Lock, Cond, Op]
		)
	of
		{ok, NewVersion} -> reply_to(ReplyTo, RequestID, {ok, NewVersion});
		{ok, NewLock, NewVersion} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, NewLock, NewVersion}
			);

		Error -> reply_to(ReplyTo, RequestID, {error, Error})
	end,
	{stop, none, {DB, ID, self()}};
handle_cast
(
	{
		request,
		ReplyTo,
		RequestID,
		{blind_update_if_then_fetch, DB, ID, Lock, Cond, Op}
	},
	_State
) ->
	case
		ataxia_network:call
		(
			DB,
			ID,
			ataxia_server,
			blind_update_if_then_fetch,
			[DB, ID, Lock, Cond, Op]
		)
	of
		{ok, Version, Value} ->
			reply_to(ReplyTo, RequestID, {ok, Version, Value}),
			{noreply, #cache_entry{ version = Version, value = Value }};

		{ok, NewLock, Version, Value} ->
			reply_to(ReplyTo, RequestID, {ok, NewLock, Version, Value}),
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
		{blind_update_if_else_fetch, DB, ID, Lock, Cond, Op}
	},
	_State
) ->
	case
		ataxia_network:call
		(
			DB,
			ID,
			ataxia_server,
			blind_update_if_else_fetch,
			[DB, Lock, ID, Cond, Op]
		)
	of
		{ok, updated, NewVersion} ->
			reply_to(ReplyTo, RequestID, {ok, updated, NewVersion}),
			{stop, none, {DB, ID, self()}};

		{ok, updated, NewLock, NewVersion} ->
			reply_to(ReplyTo, RequestID, {ok, updated, NewLock, NewVersion}),
			{stop, none, {DB, ID, self()}};

		{ok, fetch, Version, Value} ->
			reply_to(ReplyTo, RequestID, {ok, fetch, Version, Value}),
			{noreply, #cache_entry{ version = Version, value = Value }};

		{ok, fetch, NewLock, Version, Value} ->
			reply_to(ReplyTo, RequestID, {ok, fetch, NewLock, Version, Value}),
			{noreply, #cache_entry{ version = Version, value = Value }};

		Error ->
			reply_to(ReplyTo, RequestID, {error, Error}),
			{stop, none, {DB, ID, self()}}
	end;
handle_cast
(
	{request, ReplyTo, RequestID, {blind_remove_if, DB, ID, Lock, Cond}},
	State
) ->
	case
		ataxia_network:call
		(
			DB,
			ID,
			ataxia_server,
			blind_remove_if,
			[DB, ID, Lock, Cond]
		)
	of
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

-spec request
(
	pid(),
	non_neg_integer(),
	term()
) -> 'ok'.
request (EntryPID, RequestID, Request) ->
	gen_server:cast(EntryPID, {request, self(), RequestID, Request}).
