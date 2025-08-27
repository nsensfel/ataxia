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

% 5min
-define(CACHE_TIMEOUT, 300000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
	cache_entry,
	{
		db :: atom(),
		id :: ataxia_id:type(),
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
			[ataxia_network:as_proc(ReplyTo), DB, ID, Lock, Value]
		)
	of
		{ok, Version} ->
			reply_to(ReplyTo, RequestID, {ok, Version}),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					version = Version,
					value = Value
				},
				?CACHE_TIMEOUT
			};

		{ok, Lock, Version} ->
			reply_to(ReplyTo, RequestID, {ok, Lock, Version}),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					version = Version,
					value = Value
				},
				?CACHE_TIMEOUT
			};

		Error ->
			reply_to(ReplyTo, RequestID, Error),
			{stop, {shutdown, {DB, ID, self()}}, none}
	end;
handle_cast
(
	{request, ReplyTo, RequestID, {fetch, DB, ID, Lock}},
	State
) ->
	Request =
		case State of
			none -> [ataxia_network:as_proc(ReplyTo), DB, ID, Lock, none];
			Entry -> [ataxia_network:as_proc(ReplyTo), DB, ID, Lock, Entry#cache_entry.version]
		end,
	case ataxia_network:call(DB, ID, ataxia_server, fetch, Request) of
		{ok, ok} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, State#cache_entry.version, State#cache_entry.value}
			),
			{noreply, State, ?CACHE_TIMEOUT};

		{ok, NewLock, ok} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, NewLock, State#cache_entry.version, State#cache_entry.value}
			),
			{noreply, State, ?CACHE_TIMEOUT};

		{ok, NewLock, {NewVersion, NewValue}} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, NewLock, NewVersion, NewValue}
			),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					version = NewVersion,
					value = NewValue
				},
				?CACHE_TIMEOUT
			};

		{ok, {NewVersion, NewValue}} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, NewVersion, NewValue}
			),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					version = NewVersion,
					value = NewValue
				},
				?CACHE_TIMEOUT
			};

		Error ->
			reply_to(ReplyTo, RequestID, Error),
			{stop, {shutdown, {DB, ID, self()}}, none}
	end;
handle_cast
(
	{request, ReplyTo, RequestID, {fetch_if_new, DB, ID, Lock, Version, Value}},
	_State
) ->
	Request = [ataxia_network:as_proc(ReplyTo), DB, ID, Lock, Version],
	case ataxia_network:call(DB, ID, ataxia_server, fetch, Request) of
		{ok, ok} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				ok
			),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					version = Version,
					value = Value
				},
				?CACHE_TIMEOUT
			};

		{ok, NewLock, ok} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, NewLock}
			),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					version = Version,
					value = Value
				},
				?CACHE_TIMEOUT
			};

		{ok, NewLock, {NewVersion, NewValue}} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, NewLock, NewVersion, NewValue}
			),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					version = NewVersion,
					value = NewValue
				},
				?CACHE_TIMEOUT
			};

		{ok, {NewVersion, NewValue}} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, NewVersion, NewValue}
			),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					version = NewVersion,
					value = NewValue
				},
				?CACHE_TIMEOUT
			};

		Error ->
			reply_to(ReplyTo, RequestID, Error),
			{stop, {shutdown, {DB, ID, self()}}, none}
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
				[ataxia_network:as_proc(ReplyTo), DB, ID, Lock, Op]
			)
		of
			{ok, NewLock, NewVersion} -> {ok, NewLock, NewVersion};
			{ok, NewVersion} -> {ok, NewVersion};
			Error -> Error
		end
	),
	{stop, {shutdown, {DB, ID, self()}}, none};
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
	Request = [ataxia_network:as_proc(ReplyTo), DB, ID, Lock, ExpectedCurrentVersion, Op],
	case ataxia_network:call(DB, ID, ataxia_server, safe_update, Request) of
		{ok, NewLock, NewVersion} ->
			reply_to(ReplyTo, RequestID, {ok, NewLock, NewVersion}),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					value = ExpectedNewValue,
					version = NewVersion
				},
				?CACHE_TIMEOUT
			};

		{ok, NewVersion} ->
			reply_to(ReplyTo, RequestID, {ok, NewVersion}),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					value = ExpectedNewValue,
					version = NewVersion
				},
				?CACHE_TIMEOUT
			};

		Error ->
			reply_to(ReplyTo, RequestID, Error),
			{stop, {shutdown, {DB, ID, self()}}, none}
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
	{stop, {shutdown, {DB, ID, self()}}, none};
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
	Request = [ataxia_network:as_proc(ReplyTo), DB, ID, Lock, ExpectedCurrentVersion, Op],
	case ataxia_network:call(DB, ID, ataxia_server, safe_update, Request) of
		{ok, NewLock, NewVersion} ->
			reply_to(ReplyTo, RequestID, {ok, NewLock, NewVersion}),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					value = ExpectedNewValue,
					version = NewVersion
				},
				?CACHE_TIMEOUT
			};

		{ok, NewVersion} ->
			reply_to(ReplyTo, RequestID, {ok, NewVersion}),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					value = ExpectedNewValue,
					version = NewVersion
				},
				?CACHE_TIMEOUT
			};

		Error ->
			reply_to(ReplyTo, RequestID, Error),
			{stop, {shutdown, {DB, ID, self()}}, none}
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
	erlang:display("BUTF: server call..."),
	case
		ataxia_network:call
		(
			DB,
			ID,
			ataxia_server,
			blind_update_then_fetch,
			[ataxia_network:as_proc(ReplyTo), DB, ID, Lock, Op]
		)
	of
		{ok, NewLock, {Version, Value}} ->
			reply_to(ReplyTo, RequestID, {ok, NewLock, Version, Value}),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					value = Value,
					version = Version
				},
				?CACHE_TIMEOUT
			};

		{ok, {Version, Value}} ->
			reply_to(ReplyTo, RequestID, {ok, Version, Value}),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					value = Value,
					version = Version
				},
				?CACHE_TIMEOUT
			};

		Error ->
			reply_to(ReplyTo, RequestID, Error),
			{stop, {shutdown, {DB, ID, self()}}, none}
	end;
handle_cast
(
	{request, ReplyTo, RequestID, {blind_remove, DB, ID, Lock}},
	_State
) ->
	case
		ataxia_network:call
		(
			DB,
			ID,
			ataxia_server,
			blind_remove,
			[ataxia_network:as_proc(ReplyTo), DB, ID, Lock]
		)
	of
		{ok, ok} -> reply_to(ReplyTo, RequestID, ok);
		Error -> reply_to(ReplyTo, RequestID, Error)
	end,
	{stop, {shutdown, {DB, ID, self()}}, none};
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
			[ataxia_network:as_proc(ReplyTo), DB, ID, Lock, ExpectedCurrentVersion]
		)
	of
		{ok, ok} -> reply_to(ReplyTo, RequestID, ok);
		Error -> reply_to(ReplyTo, RequestID, Error)
	end,
	{stop, {shutdown, {DB, ID, self()}}, none};
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
	{stop, {shutdown, {DB, ID, self()}}, none};
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
			[ataxia_network:as_proc(ReplyTo), DB, ID, Lock, ExpectedCurrentVersion]
		)
	of
		{ok, ok} -> reply_to(ReplyTo, RequestID, ok);
		Error -> reply_to(ReplyTo, RequestID, Error)
	end,
	{stop, {shutdown, {DB, ID, self()}}, none};
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
			none -> [ataxia_network:as_proc(ReplyTo), DB, ID, Lock, none, Cond];
			Entry -> [ataxia_network:as_proc(ReplyTo), DB, ID, Lock, Entry#cache_entry.version, Cond]
		end,
	case ataxia_network:call(DB, ID, ataxia_server, fetch_if, Request) of
		{ok, ok} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, State#cache_entry.version, State#cache_entry.value}
			),
			{noreply, State, ?CACHE_TIMEOUT};

		{ok, {NewVersion, NewValue}} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, NewVersion, NewValue}
			),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					value = NewValue,
					version = NewVersion
				},
				?CACHE_TIMEOUT
			};

		{ok, NewLock, ok} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, NewLock, State#cache_entry.version, State#cache_entry.value}
			),
			{noreply, State, ?CACHE_TIMEOUT};

		{ok, NewLock, {NewVersion, NewValue}} ->
			reply_to
			(
				ReplyTo,
				RequestID,
				{ok, NewLock, NewVersion, NewValue}
			),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					value = NewValue,
					version = NewVersion
				},
				?CACHE_TIMEOUT
			};

		{error, condition} ->
			reply_to(ReplyTo, RequestID, {error, condition}),
			{noreply, State, ?CACHE_TIMEOUT};

		Error ->
			reply_to(ReplyTo, RequestID, Error),
			{stop, {shutdown, {DB, ID, self()}}, none}
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
			blind_update_if,
			[ataxia_network:as_proc(ReplyTo), DB, ID, Lock, Cond, Op]
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

		Error -> reply_to(ReplyTo, RequestID, Error)
	end,
	{stop, {shutdown, {DB, ID, self()}}, none};
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
			[ataxia_network:as_proc(ReplyTo), DB, ID, Lock, Cond, Op]
		)
	of
		{ok, {Version, Value}} ->
			reply_to(ReplyTo, RequestID, {ok, Version, Value}),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					value = Value,
					version = Version
				},
				?CACHE_TIMEOUT
			};

		{ok, NewLock, {Version, Value}} ->
			reply_to(ReplyTo, RequestID, {ok, NewLock, Version, Value}),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					value = Value,
					version = Version
				},
				?CACHE_TIMEOUT
			};

		Error ->
			reply_to(ReplyTo, RequestID, Error),
			{stop, {shutdown, {DB, ID, self()}}, none}
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
			[ataxia_network:as_proc(ReplyTo), DB, Lock, ID, Cond, Op]
		)
	of
		{ok, {updated, NewVersion}} ->
			reply_to(ReplyTo, RequestID, {ok, updated, NewVersion}),
			{stop, {shutdown, {DB, ID, self()}}, none};

		{ok, NewLock, {updated, NewVersion}} ->
			reply_to(ReplyTo, RequestID, {ok, updated, NewLock, NewVersion}),
			{stop, {shutdown, {DB, ID, self()}}, none};

		{ok, {fetch, Version, Value}} ->
			reply_to(ReplyTo, RequestID, {ok, fetch, Version, Value}),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					value = Value,
					version = Version
				},
				?CACHE_TIMEOUT
			};

		{ok, NewLock, {fetch, Version, Value}} ->
			reply_to(ReplyTo, RequestID, {ok, fetch, NewLock, Version, Value}),
			{
				noreply,
				#cache_entry
				{
					db = DB,
					id = ID,
					value = Value,
					version = Version
				},
				?CACHE_TIMEOUT
			};

		Error ->
			reply_to(ReplyTo, RequestID, Error),
			{stop, {shutdown, {DB, ID, self()}}, none}
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
			[ataxia_network:as_proc(ReplyTo), DB, ID, Lock, Cond]
		)
	of
		{ok, ok} ->
			reply_to(ReplyTo, RequestID, ok),
			{stop, {shutdown, {DB, ID, self()}}, none};

		{error, condition} ->
			reply_to(ReplyTo, RequestID, {error, condition}),
			{noreply, State, ?CACHE_TIMEOUT};

		Error ->
			reply_to(ReplyTo, RequestID, Error),
			{stop, {shutdown, {DB, ID, self()}}, none}
	end.

handle_call (_Request, _From, State) ->
	{noreply, State, ?CACHE_TIMEOUT}.

terminate (_, _) -> ok.

code_change (_, State, _) ->
	{ok, State}.

format_status (_, [_, State]) ->
	[{data, [{"State", State}]}].

handle_info(timeout, none) ->
	{
		stop,
		{shutdown, {none, none, self()}},
		none
	};
handle_info(timeout, State) ->
	{
		stop,
		{shutdown, {State#cache_entry.db, State#cache_entry.id, self()}},
		none
	};
handle_info(_, State) ->
	{noreply, State}.

%%%% Interface Functions
-spec start () -> pid().
start () ->
	{ok, PID} = gen_server:start_link(?MODULE, none, []),
	PID.

-spec request
(
	pid(),
	non_neg_integer(),
	term()
) -> 'ok'.
request (EntryPID, RequestID, Request) ->
	gen_server:cast(EntryPID, {request, self(), RequestID, Request}).
