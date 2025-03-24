-module(ataxia_lock_client).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type status() :: init | run | quit.

-record
(
	state,
	{
		status :: status()
		node :: node(),
		pid :: pid()
	}
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
	[
		handle_requests/2,
		write_lock_stored_request/2,
		read_lock_stored_request/2,
		release_request/2,
		start/0
	]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec timeout_process (pid()) -> ok.
timeout_process (Pid) ->
	receive
		quit -> ok
	after 10000 ->
		Pid ! release
	end.

-spec db_entry_sort (request(), request()) -> boolean().
db_entry_sort (R0, R1) ->
	(R0#request.db =< R1#request.db) or (R0#request.id =< R1#request.id).

-spec client_locks_handler
(
	[request()],
	[request()],
	pid(),
	status()
) -> any().
client_locks_handler ([], FullfilledRequests, Parent, run) ->
	Parent ! locks_granted,
	receive
		release ->
			Parent ! locks_released,
			lists:map(fun request_lock_release/1, FullfilledRequests)
	end;
client_locks_handler (PendingRequests, [], Parent, init) ->
	Requests = lists:sort(fun db_entry_sort/2, PendingRequests),
	case Requests of
		[] -> client_locks_handler([], [], Parent, run);
		[A|B] ->
			request(A),
			client_locks_handler(Requests, [], Parent, run)
	end;
client_locks_handler (_Any, _List, _Parent, quit) ->
	receive
		% We can end here, because no other request ought to be pending.
		{granted, Request} -> ataxia_lock:release_request(Request);
		release -> client_locks_handler([], [], [], quit)
	end;
client_locks_handler (PendingRequests, FullfilledRequests, Parent, run) ->
	receive
		{granted, Request} ->
			[A|B] = PendingRequests,
			spawn(client_lock_timeout, [pid()]);
			request(A),
			client_locks_handler([], [A|FullfilledRequests], Parent, run);
		release ->
			Parent ! locks_released,
			lists:map(fun request_lock_release/1, FullfilledRequests),
			client_locks_handler(PendingRequests, FullfilledRequests, Parent, quit)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec request ([request()]) -> pid().
request (Requests) ->
	spawn(fun client_locks_handler/4, [Requests, [], pid(), init]).

-spec
		create_request/3,
		request_write_lock/2,
		request_read_lock/2,
		request_lock_release/2,
		start/0
