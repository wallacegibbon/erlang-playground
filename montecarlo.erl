-module(montecarlo).

-export([pi/1]).

calc(N, F) when is_integer(N), N > 0 ->
	calc(N, F, 0) / N.

calc(N, F, In) when N > 0 ->
	calc(N - 1, F, In + bool2int(F()));
calc(0, _, In) ->
	In.

bool2int(false) -> 0;
bool2int(true) -> 1.


pi_calc() ->
	{X, Y} = {rand:uniform(), rand:uniform()},
	math:sqrt(X*X + Y*Y) < 1.

pi(N) ->
	calc(N, fun pi_calc/0) * 4.

