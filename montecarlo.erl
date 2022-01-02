-module(montecarlo).

-export([pi/1]).

-spec calc(fun(() -> 0 | 1), pos_integer()) -> pos_integer().
calc(F, N) when is_integer(N), N > 0 ->
    calc(F, N, 0) / N.

calc(F, N, In) when N > 0 ->
    calc(F, N - 1, F() + In);
calc(_, 0, In) ->
    In.

calc_pi() ->
    {X, Y} = {rand:uniform(), rand:uniform()},
    bool2int(math:sqrt(X * X + Y * Y) < 1).

bool2int(false) ->
    0;
bool2int(true) ->
    1.

pi(N) ->
    calc(fun calc_pi/0, N) * 4.
