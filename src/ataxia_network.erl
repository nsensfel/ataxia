call(DB, ID, Module, Fun, Params) ->
	% (DB, ID) selects the node, then it's a remote call.

cast(DB, ID, Module, Fun, Params) ->
	% (DB, ID) selects the node, then it's a remote cast.
