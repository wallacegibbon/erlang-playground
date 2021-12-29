-module(calculator).

-export([generate_code/1, interpret/2, run/1]).

interpret(['+' | Rest], [Op2, Op1 | Stack]) ->
    interpret(Rest, [Op1 + Op2 | Stack]);
interpret(['-' | Rest], [Op2, Op1 | Stack]) ->
    interpret(Rest, [Op1 - Op2 | Stack]);
interpret(['*' | Rest], [Op2, Op1 | Stack]) ->
    interpret(Rest, [Op1 * Op2 | Stack]);
interpret(['/' | Rest], [Op2, Op1 | Stack]) ->
    interpret(Rest, [Op1 / Op2 | Stack]);
interpret([push, I | Rest], Stack) ->
    interpret(Rest, [I | Stack]);
interpret([], [Rest | _]) ->
    Rest.

generate_code_and_flatten(Instructions) ->
    lists:flatten(generate_code(Instructions)).

generate_code({op, _, '+', Op1, Op2}) ->
    [generate_code(Op1), generate_code(Op2), '+'];
generate_code({op, _, '-', Op1, Op2}) ->
    [generate_code(Op1), generate_code(Op2), '-'];
generate_code({op, _, '*', Op1, Op2}) ->
    [generate_code(Op1), generate_code(Op2), '*'];
generate_code({op, _, '/', Op1, Op2}) ->
    [generate_code(Op1), generate_code(Op2), '/'];
generate_code({op, Line, OP, _, _}) ->
    throw({unknown_operation, OP, Line});
generate_code({integer, _, Num}) ->
    [push, Num];
generate_code({float, _, Num}) ->
    [push, Num];
generate_code(Token) ->
    throw({unknown_token, element(1, Token), element(2, Token)}).

compile(ExprString) ->
    {ok, Tokens, _} = erl_scan:string(ExprString),
    {ok, [AST]} = erl_parse:parse_exprs(Tokens),
    Code = generate_code_and_flatten(AST),
    io:format("Tokens: ~w~n~nAST: ~p~n~nCode: ~w~n", [Tokens, AST, Code]),
    Code.

run(ExprString) ->
    interpret(compile(ExprString), []).
