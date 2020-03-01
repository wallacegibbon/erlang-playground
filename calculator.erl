-module(calculator).

-compile(export_all).

compile(ExprString) ->
    {ok, Tokens, _} = erl_scan:string(ExprString),
    {ok, [AST]} = erl_parse:parse_exprs(Tokens),
    Code = generate_code(AST),
    io:format("Tokens: ~w~n~nAST: ~p~n~nCode: ~w~n", [Tokens, AST, Code]),
    Code.

generate_code({op, _Line, '+', Arg1, Arg2}) ->
    generate_code(Arg1) ++ generate_code(Arg2) ++ [add];
generate_code({op, _Line, '*', Arg1, Arg2}) ->
    generate_code(Arg1) ++ generate_code(Arg2) ++ [mul];
generate_code({op, _Line, '/', Arg1, Arg2}) ->
    generate_code(Arg1) ++ generate_code(Arg2) ++ ['div'];
generate_code({op, Line, Op, _, _}) ->
    exit_error("Unsupported operator in line ~w: ~w", [Line, Op]);
generate_code({integer, _Line, I}) ->
    [push, I];
generate_code({float, _Line, F}) ->
    [push, F].

interpret([push, I | Rest], Stack) ->
    interpret(Rest, [I | Stack]);
interpret([add | Rest], [Arg2, Arg1 | Stack]) ->
    interpret(Rest, [Arg1 + Arg2 | Stack]);
interpret([mul | Rest], [Arg2, Arg1 | Stack]) ->
    interpret(Rest, [Arg1 * Arg2 | Stack]);
interpret(['div' | Rest], [Arg2, Arg1 | Stack]) ->
    interpret(Rest, [Arg1 / Arg2 | Stack]);
interpret([], [Rest | _]) ->
    Rest.

exit_error(Fmt, Values) ->
    erlang:error(lists:flatten(io_lib:format(Fmt, Values))).

run(ExprString) ->
    interpret(compile(ExprString), []).

