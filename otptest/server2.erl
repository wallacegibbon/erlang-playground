-module(server2).

-export([rpc/2, start/2]).

start(Name, Mod) ->
    register(Name,
             spawn(fun () -> loop(Name, Mod, Mod:init()) end)).

loop(Name, Mod, OldState) ->
    receive
        {From, Request} ->
            try Mod:handle(Request, OldState) of
                {Response, NewState} ->
                    From ! {Name, ok, Response},
                    loop(Name, Mod, NewState)
            catch
                _:Why ->
                    log_the_error(Name, Request, Why),
                    From ! {Name, crash},
                    loop(Name, Mod, OldState)
            end
    end.

log_the_error(Name, Request, Why) ->
    io:format("Server ~p request ~p ~ncaused exception ~p~n",
              [Name, Request, Why]).

rpc(Name, Request) ->
    Name ! {self(), Request},
    receive
        {Name, ok, Response} ->
            Response;
        {Name, crash} ->
            exit(rpc)
    end.
