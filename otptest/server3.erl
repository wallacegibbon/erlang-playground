-module(server3).
-export([rpc/2, start/2, swap_code/2]).

start(Name, Mod) ->
	register(
		Name,
		spawn(fun () -> loop(Name, Mod, Mod:init()) end)
	).

loop(Name, Mod, OldState) ->
	receive
	{From, {swap_code, NewCallBackMod}} ->
		From ! {Name, ack},
		loop(Name, NewCallBackMod, OldState);
	{From, Request} ->
		{Resp, NewState} = Mod:handle(Request, OldState),
		From ! {Name, Resp},
		loop(Name, Mod, NewState)
	end.

rpc(Name, Req) ->
	Name ! {self(), Req},
	receive
	{Name, Resp} ->
		Resp
	end.

swap_code(Name, Mod) ->
	rpc(Name, {swap_code, Mod}).

