-module(hw).
-compile(export_all).

display(Fmt, Arguments) ->
	io:format("Display:" ++ Fmt ++ "~n", Arguments).

display(String) ->
	io:format("Display:" ++ String ++ "~n").

return_change(Payment) ->
	io:format("Machine:Returned ~w in change~n", [Payment]).

drop_cup() ->
	io:format("Machine:Dropped Cup.~n").

prepare(Type) ->
	io:format("Machine:Preparing ~p.~n", [Type]).

reboot() ->
	io:format("Machine:Rebooted Hardware~n").

