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
    {Size, prvParseChunks( readChunks(Chunks) )}.

readChunks(Chunks) -> readChunks(Chunks, []).

readChunks(<<N, A, M, E, Size:32/integer, Tail/binary>>, Acc) ->
    ChunkLength = prvAlignByFour(Size),
    <<Chunk:ChunkLength/binary, Rest/binary>> = Tail,
    readChunks(Rest, [{[N, A, M, E], Size, Chunk} | Acc]);
readChunks(<<>>, Acc) ->
    lists:reverse(Acc).

prvAlignByFour(N) -> (N + 3) div 4 * 4.

prvParseChunks(Chunks) -> prvParseChunks(Chunks, []).

prvParseChunks([{"Atom", _Size, <<_Numberofatoms:32/integer, Atoms/binary>>} | Rest], Acc) ->
    prvParseChunks(Rest, [{atoms, prvParseAtoms(Atoms)} | Acc]);
prvParseChunks([{"AtU8", Size, Atoms} | Rest], Acc) ->
    prvParseChunks([{"Atom", Size, Atoms} | Rest], Acc);
prvParseChunks([{"ExpT", _Size, <<_Numberofentries:32/integer, Exports/binary>>} | Rest], Acc) ->
    prvParseChunks(Rest, [{exports, prvParseExports(Exports)} | Acc]);
prvParseChunks([{"Code", Size, <<Subsize:32/integer, Chunk/binary>>} | Rest], Acc) ->
    <<Info:Subsize/binary, Code/binary>> = Chunk,
    %% 8 is size of CunkSize & Subsize
    OpcodeSize = Size - Subsize - 8,
    <<Opcodes:OpcodeSize/binary, _Align/binary>> = Code,
    prvParseChunks(Rest, [{code, prvParseCodeInfo(Info), Opcodes} | Acc]);
prvParseChunks([{"StrT", _Size, <<Strings/binary>>} | Rest], Acc) ->
    prvParseChunks(Rest, [{string, binary_to_list(Strings)} | Acc]);
prvParseChunks([{"Attr", Size, Chunk} | Rest], Acc) ->
    <<Bin:Size/binary, _Pad/binary>> = Chunk,
    Attribs = binary_to_term(Bin),
    prvParseChunks(Rest, [{attributes, Attribs} | Acc]);
prvParseChunks([{"CInf", Size, Chunk} | Rest], Acc) ->
    <<Bin:Size/binary, _Pad/binary>> = Chunk,
    CInfo = binary_to_term(Bin),
    prvParseChunks(Rest, [{compile_info, CInfo} | Acc]);
prvParseChunks([{"LitT", _Chunksize, <<_CompressedTableSize:32, Compressed/binary>>} | Rest], Acc) ->
    <<_NumLiterals:32, Table/binary>> = zlib:uncompress(Compressed),
    Literals = prvParseLiterals(Table),
    prvParseChunks(Rest, [{literals, Literals} | Acc]);
prvParseChunks([{"Abst", _Chunksize, <<>>} | Rest], Acc) ->
    prvParseChunks(Rest, Acc);
prvParseChunks([{"Abst", _Chunksize, <<AbstractCode/binary>>} | Rest], Acc) ->
    prvParseChunks(Rest, [{abstract_code, binary_to_term(AbstractCode)} | Acc]);
prvParseChunks([Chunk | Rest], Acc) ->
    prvParseChunks(Rest, [Chunk | Acc]);
prvParseChunks([], Acc) ->
    Acc.

prvParseExports(<<Function:32/integer, Arity:32/integer, Label:32/integer, Rest/binary>>)   -> [{Function, Arity, Label} | prvParseExports(Rest)];
prvParseExports(_Alignment)                                                                 -> [].

prvParseAtoms(<<Atomlength, Atom:Atomlength/binary, Rest/binary>>) when Atomlength > 0  -> [list_to_atom(binary_to_list(Atom)) | prvParseAtoms(Rest)];
prvParseAtoms(_Alignment)                                                               -> [].

prvParseCodeInfo(<<Instructionset:32/integer, OpcodeMax:32/integer, NumberOfLabels:32/integer, NumberOfFunctions:32/integer, Rest/binary>>) ->
    Left = case Rest of
               <<>>         -> [];
               _            -> [{newinfo, Rest}]
           end,
    [{instructionset, Instructionset}, {opcodemax, OpcodeMax}, {numoflabels, NumberOfLabels}, {numoffunctions, NumberOfFunctions} | Left].

prvParseLiterals(<<Size:32, Literal:Size/binary, Tail/binary>>)     -> [binary_to_term(Literal) | prvParseLiterals(Tail)];
prvParseLiterals(<<>>)                                              -> [].
