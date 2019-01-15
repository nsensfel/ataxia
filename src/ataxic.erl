-module(ataxic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% BASIC OP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Select
-record(field, {ix :: non_neg_integer(), op :: basic()}).
-record(upfield, {ix :: non_neg_integer(), op :: basic()}).

%%%% Sequence of instructions
-record(seq, {ops :: list(basic())}).

%%%% List
-record(const, {value :: any()}).
-record(current, {}).

-record
(
   apply_fun,
   {
      module :: atom(),
      function :: atom(),
      params :: list(basic())
   }
).

%%%% Number Comparison
-record(gt, {p0 :: basic(), p1 :: basic()}).
-record(ge, {p0 :: basic(), p1 :: basic()}).
-record(lt, {p0 :: basic(), p1 :: basic()}).
-record(le, {p0 :: basic(), p1 :: basic()}).
-record(eq, {p0 :: basic(), p1 :: basic()}).

%%%% Bool Operations
-record(land, {params :: list(basic())}).
-record(lor, {params :: list(basic())}).
-record(neg, {param :: basic()}).

-record(list_cons, {param :: basic()}).

% TODO: list all of the above.
-type basic() :: any().

%%%% META OP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Select
-record(read_perm, {op :: basic()}).
-record(write_perm, {op :: basic()}).
-record(lock, {op :: basic()}).
-record(value, {op :: basic()}).

-record(mseq, {ops :: list(meta())}).

-type meta() :: #read_perm{} | #write_perm{} | #value{} | #lock{} | #mseq{}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([basic/0, meta/0]).

-export
(
   [
      update_field/2,
      field/2,
      apply_function/3,
      sequence/1,
      constant/1,
      current_value/0,
      ge/2,
      gt/2,
      le/2,
      lt/2,
      eq/2,
      land/1,
      lor/1,
      neg/1,
      list_cons/1
   ]
).

-export
(
   [
      update_read_permission/1,
      update_write_permission/1,
      update_lock/1,
      update_value/1,
      sequence_meta/1
   ]
).

-export([apply_to/2, matches/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec basic_apply_to (basic(), any()) -> any().
basic_apply_to (#upfield{ ix = IX, op = OP}, Val) ->
   setelement(IX, Val, basic_apply_to(OP, element(IX, Val)));
basic_apply_to (#field{ ix = IX, op = OP}, Val) ->
   basic_apply_to(OP, element(IX, Val));
basic_apply_to (#apply_fun{ module = M, function = F, params = P }, Val) ->
   erlang:apply
   (
      M,
      F,
      lists:map(fun (Param) -> basic_apply_to(Param, Val) end, P)
   );

basic_apply_to (#seq{ ops = List }, Val) ->
   lists:foldl(fun basic_apply_to/2, Val, List);

basic_apply_to (#const{ value = Val }, _Val) ->
   Val;
basic_apply_to (#current{}, Val) ->
   Val;

basic_apply_to (#ge{ p0 = P0, p1 = P1 }, Val) ->
   basic_apply_to(P0, Val) >= basic_apply_to(P1, Val);
basic_apply_to (#gt{ p0 = P0, p1 = P1 }, Val) ->
   basic_apply_to(P0, Val) > basic_apply_to(P1, Val);
basic_apply_to (#le{ p0 = P0, p1 = P1 }, Val) ->
   basic_apply_to(P0, Val) =< basic_apply_to(P1, Val);
basic_apply_to (#lt{ p0 = P0, p1 = P1 }, Val) ->
   basic_apply_to(P0, Val) < basic_apply_to(P1, Val);
basic_apply_to (#eq{ p0 = P0, p1 = P1 }, Val) ->
   basic_apply_to(P0, Val) == basic_apply_to(P1, Val);

basic_apply_to (#land{ params = List }, Val) ->
   lists:all(fun (E) -> basic_apply_to(E, Val) end, List);
basic_apply_to (#lor{ params = List }, Val) ->
   lists:any(fun (E) -> basic_apply_to(E, Val) end, List);
basic_apply_to (#neg{ param = V }, Val) ->
   not basic_apply_to(V, Val);

basic_apply_to (#list_cons{ param = V }, Val) ->
   [V|Val].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec update_field (non_neg_integer(), basic()) -> basic().
update_field (IX, OP) -> #upfield{ ix = IX, op = OP }.

-spec field (non_neg_integer(), basic()) -> basic().
field (IX, OP) -> #field{ ix = IX, op = OP }.

-spec sequence (list(basic())) -> basic().
sequence (List) -> #seq{ ops = List }.

-spec apply_function (atom(), atom(), list(basic())) -> basic().
apply_function (Module, Function, Params) ->
   #apply_fun{ module = Module, function = Function, params = Params}.

-spec constant (any()) -> basic().
constant (Val) -> #const{ value = Val }.

-spec current_value () -> basic().
current_value () -> #current{}.

-spec ge (basic(), basic()) -> basic().
ge (P0, P1) -> #ge{ p0 = P0, p1 = P1 }.

-spec gt (basic(), basic()) -> basic().
gt (P0, P1) -> #gt{ p0 = P0, p1 = P1 }.

-spec le (basic(), basic()) -> basic().
le (P0, P1) -> #le{ p0 = P0, p1 = P1 }.

-spec lt (basic(), basic()) -> basic().
lt (P0, P1) -> #lt{ p0 = P0, p1 = P1 }.

-spec eq (basic(), basic()) -> basic().
eq (P0, P1) -> #eq{ p0 = P0, p1 = P1 }.

-spec land (list(basic())) -> basic().
land (List) -> #land{ params = List }.

-spec lor (list(basic())) -> basic().
lor (List) -> #lor{ params = List }.

-spec neg (basic()) -> basic().
neg (V) -> #neg{ param = V }.


-spec list_cons (basic()) -> basic().
list_cons (V) -> #list_cons{ param = V}.


-spec sequence_meta (list(meta())) -> meta().
sequence_meta (List) -> #mseq{ ops = List }.

-spec update_read_permission (basic()) -> meta().
update_read_permission (OP) -> #read_perm{ op = OP }.

-spec update_lock (basic()) -> meta().
update_lock (OP) -> #lock{ op = OP }.

-spec update_write_permission (basic()) -> meta().
update_write_permission (OP) -> #write_perm{ op = OP }.

-spec update_value (basic()) -> meta().
update_value (OP) -> #value{ op = OP }.

%%%%% APPLY TO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply_to (meta(), ataxia_entry:type()) -> ataxia_entry:type().
apply_to (#value{ op = OP }, Entry) ->
   ataxia_entry:set_value
   (
      basic_apply_to(OP, ataxia_entry:get_value(Entry)),
      Entry
   );
apply_to (#lock{ op = OP }, Entry) ->
   ataxia_entry:set_lock
   (
      basic_apply_to(OP, ataxia_entry:get_lock(Entry)),
      Entry
   );
apply_to (#read_perm{ op = OP }, Entry) ->
   ataxia_entry:set_read_permission
   (
      basic_apply_to(OP, ataxia_entry:get_read_permission(Entry)),
      Entry
   );
apply_to (#write_perm{ op = OP }, Entry) ->
   ataxia_entry:set_write_permission
   (
      basic_apply_to(OP, ataxia_entry:get_write_permission(Entry)),
      Entry
   );
apply_to (#mseq { ops = List }, Entry) ->
   lists:foldl(fun apply_to/2, Entry, List).

-spec matches (basic(), ataxia_entry:type()) -> boolean().
matches (OP, Entry) ->
   Result = basic_apply_to(OP, Entry),
   io:format("matches test result:~p~n", [Result]),
   case basic_apply_to(OP, Entry) of
      true -> true;
      _ -> false
   end.
