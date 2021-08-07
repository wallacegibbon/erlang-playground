-module(nameServer).

-export([add/2, find/1, handle/2, init/0]).

add(Name, Place)        -> server3:rpc(name_server, {add, Name, Place}).
find(Name)              -> server3:rpc(name_server, {find, Name}).

handle({add, Name, Place}, Dict)        -> {ok, dict:store(Name, Place, Dict)};
handle({find, Name}, Dict)              -> {dict:find(Name, Dict), Dict}.

init() -> dict:new().