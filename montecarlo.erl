-module(montecarlo).

-export([calc_single/2, calc_multi/2, pi_single/1, pi_multi/1]).

-type boolean_fn() :: fun(() -> boolean()).

%% There are significant differences between the single-process version and the multi-process version.
%> timer:tc(fun montecarlo:pi_single/1, [100000000], second).
%  {13,3.1415402}
%> timer:tc(fun montecarlo:pi_multi/1, [100000000], second).
%  {4,3.14153088}

-spec calc_single(boolean_fn(), pos_integer()) -> float().
calc_single(F, N) when is_integer(N), N > 0 ->
    calc(F, N, 0) / N.

%% `N rem NumOfCores` got ignored for simplicity since `NumOfCores` is a small integer.
calc_multi(F, N) when is_integer(N), N > 0 ->
    NumOfCores = erlang:system_info(logical_processors_available),
    CountForEachCore = N div NumOfCores,
    Ns = lists:duplicate(NumOfCores, CountForEachCore),
    Self = self(),
    lists:foreach(fun(Count) -> spawn_link(fun() -> Self ! calc(F, Count, 0) / Count end) end,
                  Ns),
    Results = lists:map(fun(_) -> receive R -> R end end, Ns),
    lists:sum(Results) / NumOfCores.

calc(_, 0, Acc) ->
    Acc;
calc(F, N, Acc) ->
    calc(F, N - 1, bool2int(F()) + Acc).

bool2int(false) ->
    0;
bool2int(true) ->
    1.

calc_pi() ->
    {X, Y} = {rand:uniform(), rand:uniform()},
    math:sqrt(X * X + Y * Y) < 1.

pi_single(N) ->
    calc_single(fun calc_pi/0, N) * 4.

pi_multi(N) ->
    calc_multi(fun calc_pi/0, N) * 4.
