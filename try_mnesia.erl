-module(try_mnesia).

-include_lib("stdlib/include/qlc.hrl").

-compile(export_all).

-record(shop, {item, quantity, cost}).

-record(blah, {item, value}).

init() ->
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    {atomic, ok} = mnesia:create_table(shop, [{attributes, record_info(fields, shop)}]),
    {atomic, ok} = mnesia:create_table(blah, [{attributes, record_info(fields, blah)}]),
    mnesia:dirty_write(#shop{item = apple, quantity = 10, cost = 20}),
    mnesia:dirty_write(#blah{item = apple, value = 2}),
    ok.

test() ->
    spawn(fun test_read2/0),
    spawn(fun test_read2/0),
    spawn(fun test_read/0),
    spawn(fun test_read/0),
    ok.

sleep(Ms) ->
    receive after Ms -> true end.

test_read() ->
    {atomic, _} = mnesia:transaction(fun test_read_sub/0).

test_read_sub() ->
    [Inc] = qlc:e(qlc:q([Inc || #blah{item = apple, value = Inc} <- mnesia:table(blah)])),
    [R1] = qlc:e(qlc:q([X || X <- mnesia:table(shop)])),
    fmt("[~w] qlc in, ~p~n", [self(), R1]),
    sleep(1000),
    mnesia:write(#shop{item = apple, quantity = 10, cost = R1#shop.cost + Inc}),
    %fmt("[~w] sub, try to write table shop~n", [self()]),
    %mnesia:write(#shop{item=apple,quantity=10,cost=Inc}),
    sleep(1000),
    [R2] = qlc:e(qlc:q([X || X <- mnesia:table(shop)])),
    fmt("[~w] qlc out, ~p~n", [self(), R2]),
    ok.

test_read2() ->
    {atomic, _} = mnesia:transaction(fun test_read_sub2/0).

test_read_sub2() ->
    [#blah{value = Inc}] = mnesia:read(blah, apple),
    [R1] = mnesia:match_object(shop, #shop{_ = '_'}, read),
    fmt("[~w] match in, ~p~n", [self(), R1]),
    sleep(1000),
    mnesia:write(#shop{item = apple, quantity = 10, cost = R1#shop.cost + Inc}),
    %fmt("[~w] sub2, try to write table shop~n", [self()]),
    %mnesia:write(#shop{item=apple,quantity=10,cost=Inc}),
    sleep(1000),
    [R2] = mnesia:match_object(#shop{_ = '_'}),
    fmt("[~w] match out, ~p~n", [self(), R2]),
    ok.

fmt(Fmt, Args) ->
    T = calendar:system_time_to_rfc3339(erlang:system_time(), [{unit, nanosecond}]),
    io:format("[~s] " ++ Fmt, [T | Args]).
