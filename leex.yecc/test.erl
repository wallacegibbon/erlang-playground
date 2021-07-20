-module(test).

-compile(export_all).

test() ->
    {ok, Content} = file:read_file("./test.json"),
    {ok, Tokens, _} = jsonScaner:string(binary_to_list(Content)),
    {ok, Ast} = jsonParser:parse(Tokens),
    io:format("Tokens: ~w~nAst: ~p~n", [Tokens, Ast]),
    ok.
