-module(name_server1).
-export([add/2, all_names/0, delete/1, find/1, handle/2, init/0]).

all_names() ->
    server3:rpc(name_server, all_names).

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
handle(all_names, Dict) ->
    {dict:fetch_keys(Dict), Dict}.

init() ->
    dict:new().
