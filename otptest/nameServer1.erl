-module(nameServer1).

-export([add/2, allNames/0, delete/1, find/1, handle/2, init/0]).

allNames() ->
    server3:rpc(name_server, allNames).
delete(Name) ->
    server3:rpc(name_server, {delete, Name}).
add(Name, Place) ->
    server3:rpc(name_server, {add, Name, Place}).
find(Name) ->
    server3:rpc(name_server, {find, Name}).

handle({add, Name, Place}, Dict) ->
    {ok, dict:store(Name, Place, Dict)};
handle({find, Name}, Dict) ->
    {dict:find(Name, Dict), Dict};
handle({delete, Name}, Dict) ->
    {ok, dict:erase(Name, Dict)};
handle(allNames, Dict) ->
    {dict:fetch_keys(Dict), Dict}.

init() ->
    dict:new().
