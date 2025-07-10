-module(ataxic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("ataxia/ataxic.hrl").

%%%% BASIC OP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type type() ::
	#field{}
	| #upfield{}

%%%% Sequence of instructions
	| #seq{}

%%%% List
	| #const{}
	| #current{}

	| #apply_fun{}

%%%% Number Comparison
	| #gt{}
	| #ge{}
	| #lt{}
	| #le{}
	| #eq{}

%%%% Bool Operations
	| #land{}
	| #lor{}
	| #neg{}

%%%% List Operations
	| #list_cons{}

%%%% Condition
	| #tern{}

%%%% Memory
	| #letr{}
	| #var{}
.

%%%% META OP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type variable() :: atom().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([type/0, variable/0]).

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
		list_cons/1,
		ternary/3,
		bind/2,
		variable/1
	]
).

-export([apply_to/2]).

-export([is_constant/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply_to (type(), any(), dict:dict(variable(), type())) -> any().
apply_to (#upfield{ ix = IX, op = OP}, Val, Mem) ->
	setelement(IX, Val, apply_to(OP, element(IX, Val), Mem));
apply_to (#field{ ix = IX, op = OP}, Val, Mem) ->
	apply_to(OP, element(IX, Val), Mem);
apply_to (#apply_fun{ module = M, function = F, params = P }, Val, Mem) ->
	erlang:apply
	(
		M,
		F,
		lists:map(fun (Param) -> apply_to(Param, Val, Mem) end, P)
	);

apply_to (#seq{ ops = List }, Val, Mem) ->
	lists:foldl
	(
		fun (OP, CurrentVal) ->
			apply_to(OP, CurrentVal, Mem)
		end,
		Val,
		List
	);

apply_to (#const{ value = Val }, _Val, _Mem) ->
	Val;
apply_to (#current{}, Val, _Mem) ->
	Val;
apply_to (#var{ name = Name }, _Val, Mem) ->
	dict:fetch(Name, Mem);

apply_to (#ge{ p0 = P0, p1 = P1 }, Val, Mem) ->
	apply_to(P0, Val, Mem) >= apply_to(P1, Val, Mem);
apply_to (#gt{ p0 = P0, p1 = P1 }, Val, Mem) ->
	apply_to(P0, Val, Mem) > apply_to(P1, Val, Mem);
apply_to (#le{ p0 = P0, p1 = P1 }, Val, Mem) ->
	apply_to(P0, Val, Mem) =< apply_to(P1, Val, Mem);
apply_to (#lt{ p0 = P0, p1 = P1 }, Val, Mem) ->
	apply_to(P0, Val, Mem) < apply_to(P1, Val, Mem);
apply_to (#eq{ p0 = P0, p1 = P1 }, Val, Mem) ->
	apply_to(P0, Val, Mem) == apply_to(P1, Val, Mem);

apply_to (#land{ params = List }, Val, Mem) ->
	lists:all(fun (E) -> apply_to(E, Val, Mem) end, List);
apply_to (#lor{ params = List }, Val, Mem) ->
	lists:any(fun (E) -> apply_to(E, Val, Mem) end, List);
apply_to (#neg{ param = V }, Val, Mem) ->
	not apply_to(V, Val, Mem);

apply_to (#list_cons{ param = V }, Val, Mem) ->
	[apply_to(V, Val, Mem)|Val];

apply_to (#tern{ condition = C, then = T, else = E }, Val, Mem) ->
	case apply_to(C, Val, Mem) of
		true -> apply_to(T, Val, Mem);
		false -> apply_to(E, Val, Mem);
		Other -> error({"Expected boolean from ternary condition, got", Other})
	end;

apply_to (#letr{ bindings = Bindings, op = OP }, Val, S0Mem) ->
	S1Mem =
		lists:foldl
		(
			fun ({Key, Value}, Memory) ->
				dict:store(Key, apply_to(Value, Val, Memory), Memory)
			end,
			S0Mem,
			Bindings
		),

	apply_to(OP, Val, S1Mem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec update_field (non_neg_integer(), type()) -> type().
update_field (IX, OP) -> #upfield{ ix = IX, op = OP }.

-spec field (non_neg_integer(), type()) -> type().
field (IX, OP) -> #field{ ix = IX, op = OP }.

-spec sequence (list(type())) -> type().
sequence (List) -> #seq{ ops = List }.

-spec apply_function (atom(), atom(), list(type())) -> type().
apply_function (Module, Function, Params) ->
	#apply_fun{ module = Module, function = Function, params = Params}.

-spec constant (any()) -> type().
constant (Val) -> #const{ value = Val }.

-spec current_value () -> type().
current_value () -> #current{}.

-spec ge (type(), type()) -> type().
ge (P0, P1) -> #ge{ p0 = P0, p1 = P1 }.

-spec gt (type(), type()) -> type().
gt (P0, P1) -> #gt{ p0 = P0, p1 = P1 }.

-spec le (type(), type()) -> type().
le (P0, P1) -> #le{ p0 = P0, p1 = P1 }.

-spec lt (type(), type()) -> type().
lt (P0, P1) -> #lt{ p0 = P0, p1 = P1 }.

-spec eq (type(), type()) -> type().
eq (P0, P1) -> #eq{ p0 = P0, p1 = P1 }.

-spec land (list(type())) -> type().
land (List) -> #land{ params = List }.

-spec lor (list(type())) -> type().
lor (List) -> #lor{ params = List }.

-spec neg (type()) -> type().
neg (V) -> #neg{ param = V }.

-spec list_cons (type()) -> type().
list_cons (V) -> #list_cons{ param = V}.

-spec ternary (type(), type(), type()) -> type().
ternary (Cond, Then, Else) ->
	#tern{ condition = Cond, then = Then, else = Else }.

-spec bind (list({variable(), type()}), type()) -> type().
bind (Bindings, OP) -> #letr{ bindings = Bindings, op = OP }.

-spec variable (variable()) -> type().
variable (Name) -> #var{ name = Name }.

%%%%% APPLY TO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply_to (type(), any()) -> any().
apply_to (OP, Val) -> apply_to(OP, Val, dict:new()).

-spec is_constant (type()) -> boolean().
is_constant (#const{}) -> true;
is_constant(_) -> false.
