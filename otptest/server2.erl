-module(server2).

-export([rpc/2, start/2]).

start(Name, Mod) ->
    register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

loop(Name, Mod, OldState) ->
    receive
        {From, Req} ->
            try Mod:handle(Req, OldState) of
                {Resp, NewState} ->
                    From ! {Name, ok, Resp},
                    loop(Name, Mod, NewState)
            catch
                _:Why ->
                    io:format("Server ~p request ~p ~ncaused exception ~p~n", [Name, Req, Why]),
                    From ! {Name, crash},
                    loop(Name, Mod, OldState)
            end
    end.

rpc(Name, Req) ->
    Name ! {self(), Req},
    receive
        {Name, ok, Resp} ->
            Resp;
        {Name, crash} ->
            exit(rpc)
    end.
