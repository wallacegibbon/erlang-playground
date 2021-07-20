-module(calculator).

-compile(export_all).

compile(ExprString) ->
    {ok, Tokens, _} = erl_scan:string(ExprString),
    {ok, [AST]} = erl_parse:parse_exprs(Tokens),
    Code = generateCode(AST),
    io:format("Tokens: ~w~n~nAST: ~p~n~nCode: ~w~n", [Tokens, AST, Code]),
    Code.

generateCode({op, _Line, '+', Arg1, Arg2}) ->       generateCode(Arg1) ++ generateCode(Arg2) ++ [add];
generateCode({op, _Line, '*', Arg1, Arg2}) ->       generateCode(Arg1) ++ generateCode(Arg2) ++ [mul];
generateCode({op, _Line, '/', Arg1, Arg2}) ->       generateCode(Arg1) ++ generateCode(Arg2) ++ ['div'];
generateCode({op, Line, Op, _, _}) ->               exit_error("Unsupported operator in line ~w: ~w", [Line, Op]);
generateCode({integer, _Line, I}) ->                [push, I];
generateCode({float, _Line, F}) ->                  [push, F].

interpret([push, I | Rest], Stack) ->               interpret(Rest, [I | Stack]);
interpret([add | Rest], [Arg2, Arg1 | Stack]) ->    interpret(Rest, [Arg1 + Arg2 | Stack]);
interpret([mul | Rest], [Arg2, Arg1 | Stack]) ->    interpret(Rest, [Arg1 * Arg2 | Stack]);
interpret(['div' | Rest], [Arg2, Arg1 | Stack]) ->  interpret(Rest, [Arg1 / Arg2 | Stack]);
interpret([], [Rest | _]) ->                        Rest.

exit_error(Fmt, Values) -> erlang:error(lists:flatten(io_lib:format(Fmt, Values))).

run(ExprString) -> interpret(compile(ExprString), []).