-module(dialyzer_record_test).

-export([myfn/1, main/0]).

%% To build the dialzyer database:
%% $ dialyzer --build_plt --output_plt my.plt --apps erts kernel stdlib
%%
%% Run dialyzer to test this file:
%% $ dialyzer --plts my.plt -- dialyzer_record_test.erl

-record(myrcd, {name :: string(), age :: integer()}).

-spec myfn([#myrcd{}]) -> boolean().
myfn(MyRcds) ->
    lists:any(fun(#myrcd{age = N}) -> N > 30 end, MyRcds).

main() ->
    %myfn([{myrcd, "", a}]),
    myfn([{myrcd, "", 1}]),
    myfn([]).
