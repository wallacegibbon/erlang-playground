-module(matrix).

-export([add_vv/2, mul_mm/2, mul_mv/2, mul_vs/2]).

-type vector() :: [any()].
-type matrix() :: [vector()].

%% matrix * matrix
-spec mul_mm(matrix(), matrix()) -> matrix().
mul_mm(M1, M2) ->
    lists:map(fun(V) -> mul_mv(M1, V) end, M2).

%% matrix * vector
-spec mul_mv(matrix(), vector()) -> vector().
mul_mv(M, V) ->
    merge_vectors(mul_mv1(M, V)).

%% addition of a list of vectors
-spec merge_vectors([vector()]) -> vector().
merge_vectors([V1 | VR]) ->
    lists:foldl(fun add_vv/2, V1, VR).

-spec mul_mv1(matrix(), vector()) -> [vector()].
mul_mv1([MV | MVR], [E | ER]) ->
    [mul_vs(MV, E) | mul_mv1(MVR, ER)];
mul_mv1([], []) ->
    [].

%% vector * scalar
-spec mul_vs(vector(), integer()) -> vector().
mul_vs(V, S) ->
    lists:map(fun(E) -> E * S end, V).

%% vector + vector
-spec add_vv(vector(), vector()) -> vector().
add_vv([E1 | ER1], [E2 | ER2]) ->
    [E1 + E2 | add_vv(ER1, ER2)];
add_vv([], []) ->
    [].
