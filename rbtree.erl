-module(rbtree).

-export([insert/2, insert/3]).

-type color() :: black | red.
-type rbtree() :: {node, color(), any(), Left :: rbtree(), Right :: rbtree()} | leaf.

-spec balance(rbtree()) -> rbtree().
balance({node, black, Z, {node, red, Y, {node, red, X, A, B}, C}, D}) ->
    common_fixed(X, Y, Z, A, B, C, D);
balance({node, black, Z, {node, red, X, A, {node, red, Y, B, C}}, D}) ->
    common_fixed(X, Y, Z, A, B, C, D);
balance({node, black, X, A, {node, red, Z, {node, red, Y, B, C}, D}}) ->
    common_fixed(X, Y, Z, A, B, C, D);
balance({node, black, X, A, {node, red, Y, B, {node, red, Z, C, D}}}) ->
    common_fixed(X, Y, Z, A, B, C, D);
balance(Node) ->
    Node.

-spec common_fixed(any(), any(), any(), rbtree(), rbtree(), rbtree(), rbtree()) ->
                      rbtree().
common_fixed(X, Y, Z, A, B, C, D) ->
    {node, red, Y, {node, black, X, A, B}, {node, black, Z, C, D}}.

insert(NewValue, Tree) ->
    insert(NewValue, Tree, fun(A, B) -> A < B end).

insert(NewV, {node, Color, V, L, R}, CmpFn) ->
    NewTree =
        case CmpFn(NewV, V) of
            true ->
                {node, Color, V, insert(NewV, L, CmpFn), R};
            false ->
                {node, Color, V, L, insert(NewV, R, CmpFn)}
        end,
    balance(NewTree);
insert(NewValue, leaf, _) ->
    {node, red, NewValue, leaf, leaf}.
