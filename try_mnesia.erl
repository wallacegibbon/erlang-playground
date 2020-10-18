-module(try_mnesia).

-include_lib("stdlib/include/qlc.hrl").

-compile(export_all).

-record(shop, {item, quantity, cost}).

make_db() ->
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    {atomic, ok} = mnesia:create_table(shop, [{attributes, record_info(fields,
								       shop)}]),
    ok.

init_data() ->
    F = fun() ->
		mnesia:write(#shop{item = banana, quantity = 15, cost = 35}),
		mnesia:write(#shop{item = apple, quantity = 10, cost = 20})
	end,
    {atomic, _} = mnesia:transaction(F).

test_read_sub() ->
    R1 = qlc:e(qlc:q([X || X <- mnesia:table(shop), X#shop.cost > 10])),
    io:format(">> ~p, ~p~n", [self(), R1]),
    sleep(3000),
    mnesia:write(#shop{item = apple, quantity = 10, cost = 5}),
    R2 = qlc:e(qlc:q([X || X <- mnesia:table(shop), X#shop.cost > 10])),
    io:format("<< ~p, ~p~n", [self(), R2]).

test_read() ->
    {atomic, _} = mnesia:transaction(fun test_read_sub/0).

test_read_multi() ->
    spawn(fun test_read/0),
    spawn(fun test_read/0),
    ok.

sleep(Ms) ->
    receive after Ms -> true end.

