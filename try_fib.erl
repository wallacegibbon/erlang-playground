-module(try_fib).

-compile(export_all).
-import(matrix, [mul_mm/2, mul_mv/2]).

fib(1) -> 1;
fib(2) -> 1;
fib(N) when N > 2 -> [X, _] = mfib(N), X.

%% [fib(N+2), fib(N+1)] = [[1, 1], [1, 0]] * [fib(N+1), fib(N)]
mfib(2) -> [1, 1];
mfib(N) when N > 2 -> mul_mv(fib_fact(N - 2), mfib(2)).

fib_fact(1) -> [[1, 1], [1, 0]];
fib_fact(N) -> mul_mm([[1, 1], [1, 0]], fib_fact(N - 1)).

