%% Beam file format is based on the interchange file format (EA IFF).
%% An IFF file starts with a header followed by a number of "chunks".
%%
%% Beam files differ from standard IFF files, in that each chunk is aligned
%% on 4-byte boundary (i.e. 32 bit word) instead of on a 2-byte boundary
%% as in the IFF standard.
%% To indicate that this is not a standard IFF file the IFF header
%% is tagged with "FOR1" instead of "FORM".
%% The IFF specification suggests this tag for future extensions.

-module(beamfile).
-export([read/1]).

read(Filename) ->
	{ok, Content} = file:read_file(Filename),
	<<"FOR1", Size:32/integer, "BEAM", Chunks/binary>> = Content,
	{Size, parse_chunks(read_chunks(Chunks))}.

read_chunks(Chunks) ->
	read_chunks(Chunks, []).

read_chunks(<<N, A, M, E, Size:32/integer, Tail/binary>>, Acc) ->
	ChunkLength = align_by_four(Size),
	<<Chunk:ChunkLength/binary, Rest/binary>> = Tail,
	read_chunks(Rest, [{[N, A, M, E], Size, Chunk} | Acc]);
read_chunks(<<>>, Acc) ->
	lists:reverse(Acc).

align_by_four(N) ->
	(N + 3) div 4 * 4.

parse_chunks(Chunks) ->
	parse_chunks(Chunks, []).

parse_chunks(
	[{"Atom", _Size, <<_NumOfAtoms:32/integer, Atoms/binary>>} | Rest],
	Acc
) ->
	parse_chunks(Rest, [{atoms, parseAtoms(Atoms)} | Acc]);
parse_chunks(
	[{"AtU8", Size, Atoms} | Rest],
	Acc
) ->
	parse_chunks([{"Atom", Size, Atoms} | Rest], Acc);
parse_chunks(
	[{"ExpT", _Size, <<_NumOfEntries:32/integer, Exports/binary>>} | Rest],
	Acc
) ->
	parse_chunks(Rest, [{exports, parse_exports(Exports)} | Acc]);
parse_chunks(
	[{"Code", Size, <<SubSize:32/integer, Chunk/binary>>} | Rest],
	Acc
) ->
	<<Info:SubSize/binary, Code/binary>> = Chunk,
	%% 8 is size of ChunkSize & SubSize
	OpcodeSize = Size - SubSize - 8,
	<<Opcodes:OpcodeSize/binary, _Align/binary>> = Code,
	parse_chunks(Rest, [{code, parse_code_info(Info), Opcodes} | Acc]);
parse_chunks(
	[{"StrT", _Size, <<Strings/binary>>} | Rest],
	Acc
) ->
	parse_chunks(Rest, [{string, binary_to_list(Strings)} | Acc]);
parse_chunks(
	[{"Attr", Size, Chunk} | Rest],
	Acc
) ->
	<<Bin:Size/binary, _Pad/binary>> = Chunk,
	Attributes = binary_to_term(Bin),
	parse_chunks(Rest, [{attributes, Attributes} | Acc]);
parse_chunks(
	[{"CInf", Size, Chunk} | Rest],
	Acc
) ->
	<<Bin:Size/binary, _Pad/binary>> = Chunk,
	CInfo = binary_to_term(Bin),
	parse_chunks(Rest, [{compile_info, CInfo} | Acc]);
parse_chunks(
	[
		{
			"LitT",
			_ChunkSize,
			<<_CompressedTableSize:32, Compressed/binary>>
		}
		| Rest
	],
	Acc
) ->
	<<_NumLiterals:32, Table/binary>> = zlib:uncompress(Compressed),
	Literals = parse_literals(Table),
	parse_chunks(Rest, [{literals, Literals} | Acc]);
parse_chunks([{"Abst", _ChunkSize, <<>>} | Rest], Acc) ->
	parse_chunks(Rest, Acc);
parse_chunks([{"Abst", _ChunkSize, <<AbstractCode/binary>>} | Rest], Acc) ->
	parse_chunks(
		Rest,
		[{abstract_code, binary_to_term(AbstractCode)} | Acc]
	);
parse_chunks([Chunk | Rest], Acc) ->
	parse_chunks(Rest, [Chunk | Acc]);
parse_chunks([], Acc) ->
	Acc.

parse_exports(<<
	Function:32/integer, Arity:32/integer, Label:32/integer, Rest/binary
>>) ->
	[{Function, Arity, Label} | parse_exports(Rest)];
parse_exports(_Alignment) ->
	[].

parseAtoms(<<AtomLen, Atom:AtomLen/binary, Rest/binary>>) when AtomLen > 0 ->
	[list_to_atom(binary_to_list(Atom)) | parseAtoms(Rest)];
parseAtoms(_Alignment) ->
	[].

parse_code_info(<<
	InstructionSet:32/integer,
	OpcodeMax:32/integer,
	NumberOfLabels:32/integer,
	NumOfFns:32/integer,
	Rest/binary
>>) ->
	Left =
		case Rest of
		<<>> -> [];
		_ -> [{new_info, Rest}]
		end,
	[
		{instruction_set, InstructionSet},
		{opcode_max, OpcodeMax},
		{num_of_labels, NumberOfLabels},
		{num_of_functions, NumOfFns}
		| Left
	].

parse_literals(<<Size:32, Literal:Size/binary, Tail/binary>>) ->
	[binary_to_term(Literal) | parse_literals(Tail)];
parse_literals(<<>>) ->
	[].

