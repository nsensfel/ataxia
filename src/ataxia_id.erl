-module(ataxia_id).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type type() :: binary().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export_type([type/0]).

%%%% Actual Interface
-export
(
	[
		table_manager/0,
		next/1,
		mod/2
	]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec next_char (integer()) -> integer().
next_char ($9) -> $a;
next_char ($z) -> $A;
next_char ($Z) -> $0;
next_char (C) -> (C + 1).

-spec next_id_internal
	(
		list(integer()),
		boolean(),
		list(integer())
	)
	-> list(integer()).
next_id_internal ([], true, Result) -> [$0|Result];
next_id_internal ([], false, Result) -> Result;
next_id_internal ([$Z|Next], true, Result) ->
	next_id_internal(Next, true, [$0|Result]);
next_id_internal ([Char|Next], true, Result) ->
	next_id_internal(Next, false, [next_char(Char)|Result]);
next_id_internal ([Char|Next], false, Result) ->
	next_id_internal(Next, false, [Char|Result]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec next (type()) -> type().
next (ID) ->
	list_to_binary
	(
		next_id_internal
		(
			lists:reverse(binary:bin_to_list(ID)),
			true,
			[]
		)
	).

-spec table_manager () -> type().
table_manager () -> <<"0">>.

-spec mod (type(), non_neg_integer()) -> non_neg_integer().
mod (ID, N) ->
	binary:decode_unsigned(ID) rem N.
