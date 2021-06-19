-module(try_misc).

-compile(export_all).

try_format() -> io_lib:format("~c, ~.2f, ~s~n", [97, 3.2e+1, "hello"]).

%io:format("~c, ~.2f, ~s~n", [97, 32.0, "hello"]).

-spec factorial(pos_integer()) -> pos_integer().

factorial(1) -> 1;
factorial(N) -> N * factorial(N - 1).

-spec cnt([any()]) -> pos_integer().

cnt(S) -> cnt(S, 0).

-spec cnt([any()], non_neg_integer()) -> pos_integer().

cnt([_ | Rst], Cnt) -> cnt(Rst, Cnt + 1);
cnt([], Cnt) -> Cnt.

info(Name, [{Name, I} | _]) -> I;
info(Name, [_ | R]) -> info(Name, R);
info(_, []) -> unknown.

-spec qsort([number()]) -> [number()].

qsort(Lst) -> lists:flatten(qsort2(Lst)).

%qsort(Lst) -> lists:flatten(qsort1(Lst)).
%qsort(Lst) -> qsort2(Lst).

-spec qsort1([number()]) -> [any()].

qsort1([P | T]) -> [qsort1([X || X <- T, X < P]), P, qsort1([X || X <- T, X >= P])];
qsort1([]) -> [].

%% this quick sort is much better, as it iterates the list only once.
-spec qsort2([number()]) -> [any()].

qsort2([P | T]) ->
    {Left, Right} = lists:partition(fun (A) -> A < P end, T),
    [qsort2(Left), P, qsort2(Right)];
qsort2([]) ->
    [].

-type a() :: string() | atom().

%% neither of the following specs can make ret type same as arg type

%-spec type_test(a()) -> a().
-spec type_test(A) -> A when A :: a().

type_test(blah) ->
    "blah".

-record(blah, {a, b, c}).

rcd_() ->
    io:format("~p~n", [#blah{a = 1, _ = default}]).
