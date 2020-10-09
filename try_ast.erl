%% This is based on: http://blog.stenmans.org/theBeamBook/
%% Only Leading JSON is parsed.

-module(try_ast).
-export([parse_transform/2]).

parse_transform(AST, _Options) ->
	R = json(AST, []),
	io:format("AST: ~p~n~nResult AST:~p~n", [AST, R]),
	R.

-define(FUNCTION(Clauses), {function, Label, Name, Arity, Clauses}).

json([?FUNCTION(Clauses) | Rest], Parsed) ->
	json(Rest, [?FUNCTION(json_clauses(Clauses)) | Parsed]);
json([Other | Rest], Parsed) ->
	json(Rest, [Other | Parsed]);
json([], Parsed) ->
	lists:reverse(Parsed).

json_clauses([{clause, Line, A1, A2, Code} | Clauses]) ->
	[{clause, Line, A1, A2, json_code(Code)} | json_clauses(Clauses)];
json_clauses([]) ->
	[].

-define(JSON(Json), {bin, _, [{bin_element, _,
			       {tuple, _, [Json]}, _, _}]}).

json_code([?JSON(Json) | MoreCode]) ->
	[parse_json(Json) | json_code(MoreCode)];
json_code([Other | MoreCode]) ->
	[Other | json_code(MoreCode)];
json_code([]) ->
	[].

%% JSON Object, [] | [{Label, Term}]
parse_json({tuple, Line, Fields}) ->
	parse_json_fields(Fields, Line);

%% array
parse_json({cons, Line, Head, Tail}) ->
	{cons, Line, parse_json(Head), parse_json(Tail)};

%% string, number
parse_json({string, Line, String}) ->
	{string, Line, String};
parse_json({integer, Line, Integer}) ->
	{integer, Line, Integer};
parse_json({float, Line, Float}) ->
	{float, Line, Float};
parse_json({op, Line, '-', {Type, _, N}})
  when Type =:= integer; Type =:= float ->
	{Type, Line, -N};

%% true, false, null
parse_json({atom, Line, true}) ->
	{atom, Line, true};
parse_json({atom, Line, false}) ->
	{atom, Line, false};
parse_json({atom, Line, null}) ->
	{atom, Line, null};

parse_json({var, Line, Var}) ->
	{var, Line, Var};

parse_json({nil, Line}) ->
	{nil, Line}.


parse_json_fields([{remote, L1, Key, Value} | Rest], Line) ->
	{cons, L1, {tuple, L1, [parse_json(Key), parse_json(Value)]},
	 parse_json_fields(Rest, Line)};

parse_json_fields([], L) ->
	{nil, L}.

