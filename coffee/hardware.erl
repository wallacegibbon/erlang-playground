-module(hardware).

-compile(export_all).

display(Fmt, Arguments) ->  io:format("Display:" ++ Fmt ++ "~n", Arguments).
display(String) ->          io:format("Display:" ++ String ++ "~n").
returnChange(Payment) ->    io:format("Machine:Returned ~w in change~n", [Payment]).
dropCup() ->                io:format("Machine:Dropped Cup.~n").
prepare(Type) ->            io:format("Machine:Preparing ~p.~n", [Type]).
reboot() ->                 io:format("Machine:Rebooted Hardware~n").