-module(try_ast_json).

-compile({parse_transform, try_ast}).

-export([test/1]).

test(V) ->
    <<{{
       "name": "wallace gibbon",
       "data": {
         "Parameter": V,
         "Blah": [1,2,3,[4,5,6]]
       },
       "attr": "asdfasdfasdf"
      }}>>.

