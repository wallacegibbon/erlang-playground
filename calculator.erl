-module(calculator).

-export([generateCode/1, interpret/2, run/1]).

interpret(['+' | Rest], [Operand2, Operand1 | Stack]) ->
    interpret(Rest, [Operand1 + Operand2 | Stack]);
interpret(['-' | Rest], [Operand2, Operand1 | Stack]) ->
    interpret(Rest, [Operand1 - Operand2 | Stack]);
interpret(['*' | Rest], [Operand2, Operand1 | Stack]) ->
    interpret(Rest, [Operand1 * Operand2 | Stack]);
interpret(['/' | Rest], [Operand2, Operand1 | Stack]) ->
    interpret(Rest, [Operand1 / Operand2 | Stack]);
interpret([push, I | Rest], Stack) ->
    interpret(Rest, [I | Stack]);
interpret([], [Rest | _]) ->
    Rest.

generateCodeAndFlatten(Instructions) ->
    lists:flatten( generateCode(Instructions) ).

generateCode({op, _Line, '+', Operand1, Operand2}) ->
    [generateCode(Operand1), generateCode(Operand2), '+'];
generateCode({op, _Line, '-', Operand1, Operand2}) ->
    [generateCode(Operand1), generateCode(Operand2), '-'];
generateCode({op, _Line, '*', Operand1, Operand2}) ->
    [generateCode(Operand1), generateCode(Operand2), '*'];
generateCode({op, _Line, '/', Operand1, Operand2}) ->
    [generateCode(Operand1), generateCode(Operand2), '/'];
generateCode({op, Line, OP , _, _}) ->
    throw({unknownOperation, OP, Line});
generateCode({integer, _Line, Number}) ->
    [push, Number];
generateCode({float, _Line, Number}) ->
    [push, Number];
generateCode(Token) ->
    throw({unknownToken, element(1, Token), element(2, Token)}).

compile(ExprString) ->
    {ok, Tokens, _} = erl_scan:string(ExprString),
    {ok, [AST]} = erl_parse:parse_exprs(Tokens),
    Code = generateCodeAndFlatten(AST),
    io:format("Tokens: ~w~n~nAST: ~p~n~nCode: ~w~n", [Tokens, AST, Code]),
    Code.

run(ExprString) ->
    interpret( compile(ExprString), [] ).
