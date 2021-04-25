-module(flatten).

-export([flatten1/1, flatten2/1]).

%%----------------------------------------------------------------------
%% this is the flatten used by otp
%%----------------------------------------------------------------------
flatten2(Lst) when is_list(Lst) ->
    flatten2(Lst, []).

flatten2([H | T], Tail) when is_list(H) ->
    flatten2(H, flatten2(T, Tail));
flatten2([H | T], Tail) ->
    [H | flatten2(T, Tail)];
flatten2([], Tail) ->
    Tail.

%%----------------------------------------------------------------------
%% this is the flatten based on process
%%----------------------------------------------------------------------
flatten1(Lst) when is_list(Lst) ->
    Pid = spawn(fun () -> collector([]) end),
    flatten1(Lst, Pid),
    Ref = make_ref(),
    Pid ! {self(), Ref},
    receive {Ref, R} -> R end.

flatten1([E | Rest], Collector) when is_list(E) ->
    flatten1(E, Collector),
    flatten1(Rest, Collector);
flatten1([E | Rest], Collector) ->
    Collector ! {add, E},
    flatten1(Rest, Collector);
flatten1([], _) ->
    ok.

collector(Lst) ->
    receive
        {add, Ele} ->
            collector([Ele | Lst]);
        {Pid, Ref} ->
            Pid ! {Ref, lists:reverse(Lst)}
    end.
