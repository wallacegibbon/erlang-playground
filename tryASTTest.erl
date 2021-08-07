-module(tryASTTest).

-compile({parse_transform, tryAST}).

-export([test1/1, test2/0]).

test1(V) ->
    <<{{"name": "wallace gibbon",
        "data": {"Parameter": V, "Blah": [1, 2, 3, [4, 5, 6]]},
        "attr": "asdfasdfasdf"}}>>.

test2() ->
    [],
    <<{{}}>>.
