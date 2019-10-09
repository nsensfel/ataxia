%%%% Select
-record(field, {ix :: non_neg_integer(), op :: basic()}).
-record(upfield, {ix :: non_neg_integer(), op :: basic()}).

%%%% Sequence of instructions
-record(seq, {ops :: list(basic())}).

%%%% Values Access
-record(const, {value :: any()}).
-record(current, {}).

% Possible improvement: add support for some sort of registers.
% This would add a dict (no need for orddict here) when evaluating Ataxic
% expressions, with the following functions:
% -record(reg_store, {name :: binary(), op :: basic()}).
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

%%%% META OP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Select
-record(read_perm, {op :: basic()}).
-record(write_perm, {op :: basic()}).
-record(lock, {op :: basic()}).
-record(value, {op :: basic()}).

-record(mseq, {ops :: list(meta())}).
