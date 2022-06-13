-module(machine).
-export([start_link/0, americano/0, cappuccino/0, espresso/0, tea/0, cancel/0, cup_removed/0, pay/1]).
-export([callback_mode/0, init/1, payment/3, remove/3, selection/3]).
-behaviour(gen_statem).

-define(TIMEOUT, 10000).

cappuccino() ->
    gen_statem:cast(?MODULE, {selection, cappuccino, 150}).

americano() ->
    gen_statem:cast(?MODULE, {selection, americano, 150}).

espresso() ->
    gen_statem:cast(?MODULE, {selection, espresso, 100}).

tea() ->
    gen_statem:cast(?MODULE, {selection, tea, 100}).

pay(Coins) ->
    gen_statem:cast(?MODULE, {pay, Coins}).

cancel() ->
    gen_statem:cast(?MODULE, cancel).

cup_removed() ->
    gen_statem:cast(?MODULE, cup_removed).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

selection(cast, {selection, Type, Price}, _LoopData) ->
    hardware:display("Please pay: ~w", [Price]),
    %% state timeout is too strict, it is not friendly for people who paid slow
    {next_state, payment, {Type, Price, 0}, {timeout, ?TIMEOUT, expired_without_payment}};
selection(cast, {pay, Coin}, LoopData) ->
    hardware:return_change(Coin),
    {next_state, selection, LoopData};
selection(cast, _Other, LoopData) ->
    {next_state, selection, LoopData}.

payment(cast, {pay, Coin}, {Type, Price, Paid}) when Coin + Paid < Price ->
    NewPaid = Coin + Paid,
    hardware:display("Please pay:~w", [Price - NewPaid]),
    {next_state, payment, {Type, Price, NewPaid}, {timeout, ?TIMEOUT, expired_waiting_for_more}};
payment(cast, {pay, Coin}, {Type, Price, Paid}) when Coin + Paid >= Price ->
    NewPaid = Coin + Paid,
    hardware:display("Preparing Drink."),
    hardware:return_change(NewPaid - Price),
    hardware:drop_cup(),
    hardware:prepare(Type),
    hardware:display("Remove Drink."),
    {next_state, remove, null};
payment(cast, cancel, {_, _, Paid}) ->
    hardware:display("Make Your Selection"),
    hardware:return_change(Paid),
    {next_state, selection, null};
payment(timeout, Info, {_Type, _Price, Paid}) ->
    hardware:display("Make your selection (last fail: ~p)", [Info]),
    hardware:return_change(Paid),
    {next_state, selection, []};
payment(cast, _Other, LoopData) ->
    {next_state, payment, LoopData, {timeout, ?TIMEOUT, expired_anyway}}.

remove(cast, cup_removed, LoopData) ->
    hardware:display("Make Your Selection"),
    {next_state, selection, LoopData};
remove(cast, {pay, Coin}, LoopData) ->
    hardware:return_change(Coin),
    {next_state, remove, LoopData};
remove(cast, _Other, LoopData) ->
    {next_state, remove, LoopData}.

init([]) ->
    hardware:reboot(),
    hardware:display("Make your selection"),
    process_flag(trap_exit, true),
    {ok, selection, []}.

callback_mode() ->
    state_functions.
