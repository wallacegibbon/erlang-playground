-module(server3).

-export([rpc/2, start/2, swap_code/2]).

start(Name, Mod) ->
    register(Name,
             spawn(fun () -> loop(Name, Mod, Mod:init()) end)).

loop(Name, Mod, OldState) ->
    receive
        {From, {swap_code, NewCallBackMod}} ->
            From ! {Name, ack},
            loop(Name, NewCallBackMod, OldState);
        {From, Request} ->
            {Response, NewState} = Mod:handle(Request, OldState),
            From ! {Name, Response},
            loop(Name, Mod, NewState)
    end.

rpc(Name, Request) ->
    Name ! {self(), Request},
    receive {Name, Response} -> Response end.

swap_code(Name, Mod) ->
    rpc(Name, {swap_code, Mod}).
