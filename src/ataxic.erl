-module(ataxic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% BASIC OP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Select
-record(field, {ix :: non_neg_integer(), op :: basic()}).
-record(array_cell, {ix :: non_neg_integer(), op :: basic()}).

%%%% Sequence of instructions
-record(seq, {ops :: list(basic())}).

%%%% List
-record(list_append, {values :: list(any()), head :: boolean()}).

-record(const, {value :: any()}).
-record(current, {}).

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

% TODO: list all of the above.
-type basic() :: any().

%%%% META OP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Select
-record(read_perm, {op :: basic()}).
-record(write_perm, {op :: basic()}).
-record(value, {op :: basic()}).

-record(mseq, {ops :: list(meta())}).

-type meta() :: #read_perm{} | #write_perm{} | #value{} | #mseq{}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([basic/0, meta/0]).

-export
(
   [
      on_field/2,
      on_array_cell/2,
      sequence/1,
      append_to_list/2,
      constant/1,
      current_value/0,
      ge/2,
      gt/2,
      le/2,
      lt/2,
      eq/2,
      land/1,
      lor/1,
      neg/1
   ]
).

-export
(
   [
      read_permission/1,
      write_permission/1,
      value/1,
      sequence_meta/1
   ]
).

-export([apply_to/2, matches/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec basic_apply_to (basic(), any()) -> any().
basic_apply_to (#field{ ix = IX, op = OP}, Val) ->
   setelement(IX, Val, basic_apply_to(OP, element(IX, Val)));
basic_apply_to (#array_cell{ ix = IX, op = OP }, Val) ->
   array:set(IX, basic_apply_to(OP, array:get(IX, Val)), Val);

basic_apply_to (#seq{ ops = List }, Val) ->
   lists:foldl(fun basic_apply_to/2, Val, List);

basic_apply_to (#list_append { values = List, head = Head }, Val) ->
   case Head of
      true -> (List ++ Val);
      _ -> (Val ++ List)
   end;

basic_apply_to (#const{ value = Val }, _Val) ->
   Val;
basic_apply_to (#current{}, Val) ->
   Val;

basic_apply_to (#ge{ p0 = P0, p1 = P1 }, _Val) ->
   P0 >= P1;
basic_apply_to (#gt{ p0 = P0, p1 = P1 }, _Val) ->
   P0 > P1;
basic_apply_to (#le{ p0 = P0, p1 = P1 }, _Val) ->
   P0 =< P1;
basic_apply_to (#lt{ p0 = P0, p1 = P1 }, _Val) ->
   P0 < P1;
basic_apply_to (#eq{ p0 = P0, p1 = P1 }, _Val) ->
   P0 == P1;

basic_apply_to (#land{ params = List }, _Val) ->
   lists:all(fun (E) -> E end, List);

basic_apply_to (#lor{ params = List }, _Val) ->
   lists:any(fun (E) -> E end, List);

basic_apply_to (#neg{ param = V }, _Val) ->
   not V.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec on_field (non_neg_integer(), basic()) -> basic().
on_field (IX, OP) -> #field{ ix = IX, op = OP }.

-spec on_array_cell (non_neg_integer(), basic()) -> basic().
on_array_cell (IX, OP) -> #array_cell{ ix = IX, op = OP }.

-spec sequence (list(basic())) -> basic().
sequence (List) -> #seq{ ops = List }.

-spec append_to_list (list(any()), boolean()) -> basic().
append_to_list (List, Head) -> #list_append { values = List, head = Head }.

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



-spec sequence_meta (list(meta())) -> meta().
sequence_meta (List) -> #mseq{ ops = List }.

-spec read_permission (basic()) -> meta().
read_permission (OP) -> #read_perm{ op = OP }.

-spec write_permission (basic()) -> meta().
write_permission (OP) -> #write_perm{ op = OP }.

-spec value (basic()) -> meta().
value (OP) -> #value{ op = OP }.

%%%%% APPLY TO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply_to (meta(), ataxia_entry:type()) -> ataxia_entry:type().
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
apply_to (#value{ op = OP }, Entry) ->
   ataxia_entry:set_value
   (
      basic_apply_to(OP, ataxia_entry:get_value(Entry)),
      Entry
   );
apply_to (#mseq { ops = List }, Entry) ->
   lists:foldl(fun apply_to/2, Entry, List).

-spec matches (meta(), ataxia_entry:type()) -> boolean().
matches (#read_perm{ op = OP }, Entry) ->
   case basic_apply_to(OP, ataxia_entry:get_read_permission(Entry)) of
      true -> true;
      _ -> false
   end;
matches (#write_perm{ op = OP }, Entry) ->
   case basic_apply_to(OP, ataxia_entry:get_write_permission(Entry)) of
      true -> true;
      _ -> false
   end;
matches (#value{ op = OP }, Entry) ->
   case basic_apply_to(OP, ataxia_entry:get_value(Entry)) of
      true -> true;
      _ -> false
   end.
