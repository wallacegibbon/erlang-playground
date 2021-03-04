-module(xor_crypt).

-export([transform/2]).

transform(Content, Key)
    when is_binary(Content), is_binary(Key),
         size(Key) > 0 ->
    transform(Content, Key, []).

transform(Content, Key, Results)
    when size(Content) > size(Key) ->
    KeySize = size(Key),
    <<SubContent:KeySize/binary, RestContent/binary>> =
        Content,
    transform(RestContent,
              Key,
              [doxor(SubContent, Key, []) | Results]);
transform(<<>>, _, Results) ->
    list_to_binary(lists:reverse(Results));
transform(Content, Key, Results) ->
    transform(<<>>,
              Key,
              [doxor(Content, Key, []) | Results]).

doxor(<<A, RestContent/binary>>, <<B, RestKey/binary>>,
      Result) ->
    doxor(RestContent, RestKey, [A bxor B | Result]);
doxor(<<>>, _, Result) ->
    list_to_binary(lists:reverse(Result)).
