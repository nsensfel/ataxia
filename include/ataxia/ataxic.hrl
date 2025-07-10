%%%% Select
-record(field, {ix :: non_neg_integer(), op :: ataxic:type()}).
-record(upfield, {ix :: non_neg_integer(), op :: ataxic:type()}).

%%%% Sequence of instructions
-record(seq, {ops :: list(ataxic:type())}).

%%%% Values Access
-record(const, {value :: any()}).
-record(current, {}).

% Possible improvement: add support for some sort of registers.
% This would add a dict (no need for orddict here) when evaluating Ataxic
% expressions, with the following functions:
% -record(reg_store, {name :: binary(), op :: ataxic:type()}).
% -record(reg_load, {name :: binary()}).
% It's kind of weird to use though, cause you can't retrieve what you've stored
% at a node that was deeper in the query's tree. It can be used to move values
% further down (or horizontally) though, and there are already instructions that
% give you values found further down.

-record
(
	apply_fun,
	{
		module :: atom(),
		function :: atom(),
		params :: list(ataxic:type())
	}
).

%%%% Number Comparison
-record(gt, {p0 :: ataxic:type(), p1 :: ataxic:type()}).
-record(ge, {p0 :: ataxic:type(), p1 :: ataxic:type()}).
-record(lt, {p0 :: ataxic:type(), p1 :: ataxic:type()}).
-record(le, {p0 :: ataxic:type(), p1 :: ataxic:type()}).
-record(eq, {p0 :: ataxic:type(), p1 :: ataxic:type()}).

%%%% Bool Operations
-record(land, {params :: list(ataxic:type())}).
-record(lor, {params :: list(ataxic:type())}).
-record(neg, {param :: ataxic:type()}).

%%%% List Operations
-record(list_cons, {param :: ataxic:type()}).

%%%% Condition
-record
(
	tern,
	{
		condition :: ataxic:type(),
		then :: ataxic:type(),
		else :: ataxic:type()
	}
).

%%%% Memory
-record
(
	letr,
	{
		bindings :: list({ataxic:variable(), ataxic:type()}),
		op :: ataxic:type()
	}
).
-record (var, { name :: ataxic:variable() }).

%%%% META OP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Select
-record(read_perm, {op :: ataxic:type()}).
-record(write_perm, {op :: ataxic:type()}).
-record(lock, {op :: ataxic:type()}).
-record(value, {op :: ataxic:type()}).
