-module(montecarlo).

-export([pi/1]).

calc(N, F) when is_integer(N), N > 0 -> calc(N, 0, F) / N.

calc(0, In, _) -> In;
calc(N, In, F) -> case F() of
                      true -> calc(N - 1, In + 1, F);
                      false -> calc(N - 1, In, F)
                  end.

calc_rand() ->
    {X, Y} = {rand:uniform(), rand:uniform()},
    math:sqrt(X * X + Y * Y) < 1.

pi(N) ->
    calc(N, fun calc_rand/0) * 4.

