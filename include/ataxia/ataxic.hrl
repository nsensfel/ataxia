%%%% Select
-record(field, {ix :: non_neg_integer(), op :: ataxic:basic()}).
-record(upfield, {ix :: non_neg_integer(), op :: ataxic:basic()}).

%%%% Sequence of instructions
-record(seq, {ops :: list(ataxic:basic())}).

%%%% Values Access
-record(const, {value :: any()}).
-record(current, {}).

% Possible improvement: add support for some sort of registers.
% This would add a dict (no need for orddict here) when evaluating Ataxic
% expressions, with the following functions:
% -record(reg_store, {name :: binary(), op :: ataxic:basic()}).
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
      params :: list(ataxic:basic())
   }
).

%%%% Number Comparison
-record(gt, {p0 :: ataxic:basic(), p1 :: ataxic:basic()}).
-record(ge, {p0 :: ataxic:basic(), p1 :: ataxic:basic()}).
-record(lt, {p0 :: ataxic:basic(), p1 :: ataxic:basic()}).
-record(le, {p0 :: ataxic:basic(), p1 :: ataxic:basic()}).
-record(eq, {p0 :: ataxic:basic(), p1 :: ataxic:basic()}).

%%%% Bool Operations
-record(land, {params :: list(ataxic:basic())}).
-record(lor, {params :: list(ataxic:basic())}).
-record(neg, {param :: ataxic:basic()}).

-record(list_cons, {param :: ataxic:basic()}).

%%%% META OP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Select
-record(read_perm, {op :: ataxic:basic()}).
-record(write_perm, {op :: ataxic:basic()}).
-record(lock, {op :: ataxic:basic()}).
-record(value, {op :: ataxic:basic()}).

-record(mseq, {ops :: list(ataxic:meta())}).
