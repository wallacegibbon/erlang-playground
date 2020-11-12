-module(matrix).

-export([mul_mm/2,mul_mv/2,mul_vs/2,add_vv/2]).

%% matrix - matrix multiplication
mul_mm(M1, [V21|V2R]) ->
    [mul_mv(M1, V21)|mul_mm(M1, V2R)];
mul_mm(_, []) ->
    [].

%% matrix - vector multiplication
mul_mv(Matrix, Vector) ->
    merge_vectors(mul_mv1(Matrix, Vector)).

%% addition of a list of vectors
merge_vectors([V1|VR]) ->
    lists:foldl(fun(V, Sum) -> add_vv(V, Sum) end, V1, VR).


mul_mv1([MV|MVR], [E|ER]) ->
    [mul_vs(MV, E)|mul_mv1(MVR, ER)];
mul_mv1([], []) ->
    [].

%% vector - scalar multiplication
mul_vs([E|R], S) ->
    [E * S|mul_vs(R, S)];
mul_vs([], _) ->
    [].

%% vector - vector addition
add_vv([E1|ER1], [E2|ER2]) ->
    [E1 + E2|add_vv(ER1, ER2)];
add_vv([], []) ->
    [].

