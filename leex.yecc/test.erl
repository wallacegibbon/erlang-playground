-module(test).

-compile(export_all).

test() ->
    {ok, Content} = file:read_file("./test.json"),
    {ok, Tokens, _} = json_scaner:string(binary_to_list(Content)),
    {ok, Ast} = json_parser:parse(Tokens),
    io:format("Tokens: ~w~nAst: ~p~n", [Tokens, Ast]),
    ok.
