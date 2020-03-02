-module(montecarlo).

-export([pi/1]).

calc(N, Fn) -> calc(N, 0, Fn, Fn()) / N.

calc(0, In, _, _) -> In;
calc(N, In, Fn, true) -> calc(N - 1, In + 1, Fn, Fn());
calc(N, In, Fn, false) -> calc(N - 1, In, Fn, Fn()).

do_calc() ->
    {X, Y} = {rand:uniform(), rand:uniform()},
    math:sqrt(X * X + Y * Y) < 1.

pi(N) ->
    calc(N, fun do_calc/0) * 4.

