-module(myutil).
-export([compose/1]).

-include_lib("eunit/include/eunit.hrl").

-spec compose([ComposeFn]) -> ComposeFn when ComposeFn :: fun ((any()) -> any()).
compose([Rule | Rules]) ->
    fun (E) -> (compose(Rules))(Rule(E)) end;
compose([]) ->
    fun (E) -> E end.

-ifdef(EUNIT).

compose_test() ->
    ComponsedFn = compose([fun (A) -> A + 1 end, fun (A) -> A * 2 end]),
    ?assertEqual(8, ComponsedFn(3)).

-endif.
