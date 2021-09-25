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
    %{Size, readChunks(Chunks)}.
    {Size, parseChunks(readChunks(Chunks))}.

readChunks(Chunks) ->
    readChunks(Chunks, []).

readChunks(<<N, A, M, E, Size:32/integer, Tail/binary>>, Acc) ->
    ChunkLength = alignByFour(Size),
    <<Chunk:ChunkLength/binary, Rest/binary>> = Tail,
    readChunks(Rest, [{[N, A, M, E], Size, Chunk} | Acc]);
readChunks(<<>>, Acc) ->
    lists:reverse(Acc).

alignByFour(N) ->
    (N + 3) div 4 * 4.

parseChunks(Chunks) ->
    parseChunks(Chunks, []).

parseChunks([{"Atom", _Size, <<_Numberofatoms:32/integer, Atoms/binary>>} | Rest], Acc) ->
    parseChunks(Rest, [{atoms, parseAtoms(Atoms)} | Acc]);
parseChunks([{"AtU8", Size, Atoms} | Rest], Acc) ->
    parseChunks([{"Atom", Size, Atoms} | Rest], Acc);
parseChunks([{"ExpT", _Size, <<_Numberofentries:32/integer, Exports/binary>>} | Rest], Acc) ->
    parseChunks(Rest, [{exports, parseExports(Exports)} | Acc]);
parseChunks([{"Code", Size, <<Subsize:32/integer, Chunk/binary>>} | Rest], Acc) ->
    <<Info:Subsize/binary, Code/binary>> = Chunk,
    %% 8 is size of CunkSize & Subsize
    OpcodeSize = Size - Subsize - 8,
    <<Opcodes:OpcodeSize/binary, _Align/binary>> = Code,
    parseChunks(Rest, [{code, parseCodeInfo(Info), Opcodes} | Acc]);
parseChunks([{"StrT", _Size, <<Strings/binary>>} | Rest], Acc) ->
    parseChunks(Rest, [{string, binary_to_list(Strings)} | Acc]);
parseChunks([{"Attr", Size, Chunk} | Rest], Acc) ->
    <<Bin:Size/binary, _Pad/binary>> = Chunk,
    Attribs = binary_to_term(Bin),
    parseChunks(Rest, [{attributes, Attribs} | Acc]);
parseChunks([{"CInf", Size, Chunk} | Rest], Acc) ->
    <<Bin:Size/binary, _Pad/binary>> = Chunk,
    CInfo = binary_to_term(Bin),
    parseChunks(Rest, [{compile_info, CInfo} | Acc]);
parseChunks([{"LitT", _Chunksize, <<_CompressedTableSize:32, Compressed/binary>>} | Rest], Acc) ->
    <<_NumLiterals:32, Table/binary>> = zlib:uncompress(Compressed),
    Literals = parseLiterals(Table),
    parseChunks(Rest, [{literals, Literals} | Acc]);
parseChunks([{"Abst", _Chunksize, <<>>} | Rest], Acc) ->
    parseChunks(Rest, Acc);
parseChunks([{"Abst", _Chunksize, <<AbstractCode/binary>>} | Rest], Acc) ->
    parseChunks(Rest, [{abstract_code, binary_to_term(AbstractCode)} | Acc]);
parseChunks([Chunk | Rest], Acc) ->
    parseChunks(Rest, [Chunk | Acc]);
parseChunks([], Acc) ->
    Acc.

parseExports(<<Function:32/integer, Arity:32/integer, Label:32/integer, Rest/binary>>) ->
    [{Function, Arity, Label} | parseExports(Rest)];
parseExports(_Alignment) ->
    [].

parseAtoms(<<Atomlength, Atom:Atomlength/binary, Rest/binary>>) when Atomlength > 0 ->
    [list_to_atom(binary_to_list(Atom)) | parseAtoms(Rest)];
parseAtoms(_Alignment) ->
    [].

parseCodeInfo(<<Instructionset:32/integer, OpcodeMax:32/integer, NumberOfLabels:32/integer, NumberOfFunctions:32/integer, Rest/binary>>) ->
    Left = case Rest of
               <<>> ->
                   [];
               _ ->
                   [{newinfo, Rest}]
           end,
    [{instructionset, Instructionset}, {opcodemax, OpcodeMax}, {numoflabels, NumberOfLabels}, {numoffunctions, NumberOfFunctions} | Left].

parseLiterals(<<Size:32, Literal:Size/binary, Tail/binary>>) ->
    [binary_to_term(Literal) | parseLiterals(Tail)];
parseLiterals(<<>>) ->
    [].
