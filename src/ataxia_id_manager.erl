-module(ataxia_id_manager).
-behavior(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record
(
   entry,
   {
      last_id :: ataxia_id:type(),
      freed_ids :: list(ataxia_id:type())
   }
).

-type entry() :: #entry{}.

%% FIXME: IDs should be generated in a decentralized fashion.
%% TODO: How to handle IDs management with the redundancy DBs?

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

%%%% Actual Interface
-export
(
   [
      allocate/1,
      free/2,
      start/0
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec new_entry () -> entry().
new_entry () ->
   #entry
   {
      last_id = ataxia_id:null(),
      freed_ids = []
   }.

-spec allocate_id_in_entry (entry()) -> {entry(), ataxia_id:type()}.
allocate_id_in_entry (Entry) ->
   case Entry#entry.freed_ids of
      [] ->
         NewID = ataxia_id:next(Entry#entry.last_id),
         {
            Entry#entry{ last_id = NewID },
            NewID
         };

      [OldID|OtherOldIDs] ->
         {
            Entry#entry{ freed_ids = OtherOldIDs },
            OldID
         }
   end.

-spec allocate_id
   (
      atom(),
      dict:dict(atom(), entry())
   )
   -> {dict:dict(atom(), entry()), ataxia_id:type()}.
allocate_id (DB, State) ->
   S0Entry =
      case dict:find(DB, State) of
         {ok, Value} -> Value;
         error -> new_entry()
      end,

   {S1Entry, NewID} = allocate_id_in_entry(S0Entry),

   S0State = dict:store(DB, S1Entry, State),

   {S0State, NewID}.

-spec free_id
   (
      ataxia_id:type(),
      atom(),
      dict:dict(atom(), entry())
   )
   -> dict:dict(atom(), entry()).
free_id (ID, DB, State) ->
   Entry =
      case dict:find(DB, State) of
         {ok, Value} -> Value;
         error -> new_entry()
      end,

   EntryFreedIDs = Entry#entry.freed_ids,

   case lists:member(ID, EntryFreedIDs) of
      true -> State;
      false ->
         Result =
            dict:store
            (
               DB,
               Entry#entry{ freed_ids = [ID|EntryFreedIDs] },
               State
            ),

         Result
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 'gen_server' functions
init (_) -> {ok, dict:new()}.

handle_call ({allocate, DB}, _, State) ->
   {NewState, NewID} = allocate_id(DB, State),
   {reply, {allocated_id, NewID}, NewState};
handle_call ({free, ID, DB}, _, State) ->
   {noreply, free_id(ID, DB, State)}.

handle_cast ({allocate, _DB}, State) ->
   {noreply, State};
handle_cast ({free, ID, DB}, State) ->
   {noreply, free_id(ID, DB, State)}.

terminate (_, _) -> ok.

code_change (_, State, _) ->
   {ok, State}.

format_status (_, [_, State]) ->
   [{data, [{"State", State}]}].

handle_info(_, State) ->
   {noreply, State}.

%%%% Interface Functions
-spec allocate (atom()) -> ataxia_id:type().
allocate (DB) ->
   {allocated_id, Result} =
      gen_server:call({global, ataxia_id_manager}, {allocate, DB}),

   io:format("~n[DB: ~p] Item ID ~p allocated.~n", [DB, Result]),

   Result.

-spec free (ataxia_id:type(), atom()) -> 'ok'.
free (ID, DB) ->
   gen_server:cast({global, ataxia_id_manager}, {free, ID, DB}),

   ok.

-spec start () -> 'ok'.
start () ->
   {ok, _} = gen_server:start({global, ataxia_id_manager}, ?MODULE, none, []),

   ok.
