-module(try_misc).

-compile(export_all).

-spec factorial(pos_integer()) -> pos_integer().
factorial(1) ->
  1;
factorial(N) ->
  N * factorial(N - 1).

-spec qsort([number()]) -> [number()].
qsort(Lst) ->
  lists:flatten(qsort2(Lst)).

-spec qsort1([number()]) -> [any()].
qsort1([P | T]) ->
  [qsort1([X || X <- T, X < P]), P, qsort1([X || X <- T, X >= P])];
qsort1([]) ->
  [].

%% this one is much better, as it iterates the list only once during partition.
-spec qsort2([number()]) -> [any()].
qsort2([P | T]) ->
  {Left, Right} = lists:partition(fun(A) -> A < P end, T),
  [qsort2(Left), P, qsort2(Right)];
qsort2([]) ->
  [].

-record(blah, {a, b, c}).

rcd_() ->
  io:format("~p~n", [#blah{a = 1, _ = default}]).
