-module(machine).

-behaviour(gen_statem).

-export([americano/0, cappuccino/0, espresso/0, tea/0]).

-export([cancel/0, cup_removed/0, pay/1]).

-export([callback_mode/0, init/1]).

-export([payment/3, remove/3, selection/3]).

-export([start_link/0]).

-define(SERVER, ?MODULE).

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
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

selection(cast, {selection, Type, Price}, _LoopData) ->
    hw:display("Please pay: ~w", [Price]),
    %% state timeout is too strict, it is not friendly for people who paid slow
    %{next_state,payment,{Type,Price,0},{state_timeout,?TIMEOUT,expired}};
    {next_state, payment, {Type, Price, 0}, {timeout, ?TIMEOUT,
                                             expired_without_payment}};
selection(cast, {pay, Coin}, LoopData) ->
    hw:return_change(Coin),
    {next_state, selection, LoopData};
selection(cast, _Other, LoopData) ->
    {next_state, selection, LoopData}.

payment(cast, {pay, Coin}, {Type, Price, Paid})
        when Coin + Paid < Price ->
    NewPaid = Coin + Paid,
    hw:display("Please pay:~w", [Price - NewPaid]),
    %{next_state,payment,{Type,Price,NewPaid}};
    {next_state, payment, {Type, Price, NewPaid}, {timeout, ?TIMEOUT,
                                                   expired_waiting_for_more}};
payment(cast, {pay, Coin}, {Type, Price, Paid})
        when Coin + Paid >= Price ->
    NewPaid = Coin + Paid,
    hw:display("Preparing Drink."),
    hw:return_change(NewPaid - Price),
    hw:drop_cup(),
    hw:prepare(Type),
    hw:display("Remove Drink."),
    {next_state, remove, null};
payment(cast, cancel, {_, _, Paid}) ->
    hw:display("Make Your Selection"),
    hw:return_change(Paid),
    {next_state, selection, null};
%payment(state_timeout, _, {_Type,_Price,Paid}) ->
payment(timeout, Info, {_Type, _Price, Paid}) ->
    hw:display("Make your selection (last fail: ~p)", [Info]),
    hw:return_change(Paid),
    {next_state, selection, []};
payment(cast, _Other, LoopData) ->
    %{next_state,payment,LoopData}.
    {next_state, payment, LoopData, {timeout, ?TIMEOUT, expired_anyway}}.

remove(cast, cup_removed, LoopData) ->
    hw:display("Make Your Selection"),
    {next_state, selection, LoopData};
remove(cast, {pay, Coin}, LoopData) ->
    hw:return_change(Coin),
    {next_state, remove, LoopData};
remove(cast, _Other, LoopData) ->
    {next_state, remove, LoopData}.

init([]) ->
    hw:reboot(),
    hw:display("Make your selection"),
    process_flag(trap_exit, true),
    {ok, selection, []}.

callback_mode() ->
    state_functions.
