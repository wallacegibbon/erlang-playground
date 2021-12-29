-module(try_fibonacci).

-compile(export_all).

%% the stupid fib algorithm
fib1(N) when is_integer(N) and (N > 2) ->
    fib1(N - 1) + fib1(N - 2);
fib1(2) ->
    1;
fib1(1) ->
    1.

%% the clever fib algorithm based on matrix
%% [fib(N+2), fib(N+1)] = [[1,1], [1,0]] * [fib(N+1), fib(N)]
fib2(N) when is_integer(N) and (N > 2) ->
    [X, _] = matrix:mul_mv(fib_fact(N - 2), [1, 1]),
    X;
fib2(2) ->
    1;
fib2(1) ->
    1.

fib_fact(N) when N > 1 ->
    matrix:mul_mm([[1, 1], [1, 0]], fib_fact(N - 1));
fib_fact(1) ->
    [[1, 1], [1, 0]].
