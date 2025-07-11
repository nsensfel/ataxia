-module(ataxia_database).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
	[
		ensure_exists/1,
		write/3,
		read/2,
		delete/2
	]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_filenames
	(
		atom(),
		ataxia_id:type()
	)
	-> {binary(), binary(), binary()}.
get_filenames (DB, ID) ->
	{ok, CWD} = file:get_cwd(),
	BaseDirectory = filename:join(CWD, DB),
	BaseFilename = filename:join(BaseDirectory, ID),
	Copy0Suffix = <<".0">>,
	Copy1Suffix = <<".1">>,
	Copy0Filename = <<BaseFilename/binary, Copy0Suffix/binary>>,
	Copy1Filename = <<BaseFilename/binary, Copy1Suffix/binary>>,
	{BaseFilename, Copy0Filename, Copy1Filename}.

-spec ensure_folder_exists (atom()) -> 'ok'.
ensure_folder_exists (DB) ->
	{ok, CWD} = file:get_cwd(),
	BaseDirectory = filename:join(CWD, DB),
	ok = filelib:ensure_path(BaseDirectory),
	ok.

-spec ensure_db_exists
	(
		ataxia_client:type(),
		{atom(), ('free' | 'id')}
	)
	-> ataxia_client:type().
ensure_db_exists (S0Client, {DB, Mode}) ->
	{
		S1Client,
		_Output
	} =
		ataxia_client:add_at
		(
			S0Client,
			DB,
			ataxia_id:table_manager(),
			{temp, write},
			case Mode of
				free -> free;
				id -> ataxia_table_manager:new()
			end
		),
	S1Client.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec ensure_exists (list({atom(), ('free' | 'id')})) -> 'ok'.
ensure_exists (DBs) ->
	Client = ataxia_client:new(),
	lists:foldl
	(
		fun (DB, S0Client) ->
			ensure_db_exists(S0Client, DB)
		end,
		Client,
		DBs
	),
	ok.

-spec write (atom(), ataxia_id:type(), ataxia_entry:type()) -> 'ok'.
write (DB, ID, Entry) ->
	ensure_folder_exists(DB),
	{Base, Copy0, Copy1} = get_filenames(DB, ID),
	Copy0Data =
		case file:read_file(Copy0) of
			{ok, Binary0} -> Binary0;
			_ -> error
		end,
	Copy1Data =
		case file:read_file(Copy1) of
			{ok, Binary1} -> Binary1;
			_ -> error
		end,
	BaseData =
		case file:read_file(Base) of
			{ok, BinaryB} -> BinaryB;
			_ -> error
		end,

	PreviousValue =
		case {Copy0Data, Copy1Data, BaseData} of
			{error, error, error} -> error;
			{error, error, ValidData} -> ValidData;
			{error, Update1, _Old} -> Update1;
			{Update0, Update1, Old} ->
				case {Update0 == Update1, Update1 == Old, Update0 == Old} of
					{true, _, _} -> Update1;
					{_, true, _} -> Update1;
					{_, _, true} -> Update0
				end
		end,

	case PreviousValue == error of
		false ->
			case PreviousValue == BaseData of
				false -> file:write_file(Base, PreviousValue);
				_ -> ok
			end,
			case PreviousValue == Copy1Data of
				false -> file:write_file(Copy1, PreviousValue);
				_ -> ok
			end,
			case PreviousValue == Copy0Data of
				false -> file:write_file(Copy0, PreviousValue);
				_ -> ok
			end;

		_ -> ok
	end,

	Data = term_to_binary(Entry),
	erlang:display({Copy0, file:write_file(Copy0, Data)}),
	file:write_file(Copy1, Data),
	file:write_file(Base, Data),

	file:delete(Copy0),
	file:delete(Copy1),
	ok.

-spec read
	(
		atom(),
		ataxia_id:type()
	) -> ({'ok', ataxia_entry:type()} | 'error').
read (DB, ID) ->
	{Base, Copy0, Copy1} = get_filenames(DB, ID),
	Copy0Data =
		case file:read_file(Copy0) of
			{ok, Binary0} -> Binary0;
			_ -> error
		end,
	Copy1Data =
		case file:read_file(Copy1) of
			{ok, Binary1} -> Binary1;
			_ -> error
		end,
	BaseData =
		case file:read_file(Base) of
			{ok, BinaryB} -> BinaryB;
			_ -> error
		end,
	case {Copy0Data, Copy1Data, BaseData} of
		{error, error, error} -> error;
		{error, error, ValidData} -> {ok, binary_to_term(ValidData)};
		{error, Update1, _Old} -> {ok, binary_to_term(Update1)};
		{Update0, Update1, Old} ->
			case {Update0 == Update1, Update1 == Old, Update0 == Old} of
				{true, _, _} -> {ok, binary_to_term(Update1)};
				{_, true, _} -> {ok, binary_to_term(Update1)};
				{_, _, true} -> {ok, binary_to_term(Update0)}
			end
	end.

-spec delete (atom(), ataxia_id:type()) -> ('ok' | 'error').
delete (DB, ID) ->
	case file:delete(filename:join(DB, ID)) of
		ok -> ok;
		_ -> error
	end.
