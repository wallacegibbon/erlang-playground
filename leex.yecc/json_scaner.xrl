Definitions.

Digit = [0-9]
Digit1to9 = [1-9]
HexDigit = [0-9a-f]
UnescapedChar = [^\"\\]
EscapedChar = \\\\|\\\"|\\b|\\f|\\n|\\r|\\t|\\/
Unicode = \\u{HexDigit}{HexDigit}{HexDigit}{HexDigit}
Quote = [\"]
Delim = [\[\]:,{}]
Space = [\n\s\t\r]

Rules.

{Quote}{Quote} : {token,{string,TokenLine,""}}.
{Quote}({EscapedChar}|{UnescapedChar}|{Unicode})+{Quote} : {token, {string, TokenLine, fixchars(drop_quotes(TokenChars))}}.

null : {token, {null, TokenLine}}.
true : {token, {true, TokenLine}}.
false : {token, {false, TokenLine}}.
{Delim} : {token, {list_to_atom(TokenChars), TokenLine}}.
{Space} : skip_token.

-?{Digit1to9}+{Digit}*\.{Digit}+((E|e)(\+|\-)?{Digit}+)? : {token, {number, TokenLine, list_to_float(TokenChars)}}.

-?{Digit1to9}+{Digit}* : {token, {number, TokenLine, list_to_integer(TokenChars)+0.0}}.

Erlang code.

drop_quotes([$" | QuotedString]) -> lists:droplast(QuotedString).

fixchars([$\\, $" | Rest]) ->
    [$"  | fixchars(Rest)];
fixchars([$\\, $\\ | Rest]) ->
    [$\\ | fixchars(Rest)];
fixchars([$\\, $/ | Rest]) ->
    [$/  | fixchars(Rest)];
fixchars([$\\, $b | Rest]) ->
    [$\b | fixchars(Rest)];
fixchars([$\\, $f | Rest]) ->
    [$\f | fixchars(Rest)];
fixchars([$\\, $n | Rest]) ->
    [$\n | fixchars(Rest)];
fixchars([$\\, $r | Rest]) ->
    [$\r | fixchars(Rest)];
fixchars([$\\, $t | Rest]) ->
    [$\t | fixchars(Rest)];
fixchars([$\\, $u, D0, D1, D2, D3 | Rest]) ->
    [list_to_integer([D0, D1, D2, D3], 16) | fixchars(Rest)];
fixchars([C | Rest]) ->
    [C   | fixchars(Rest)];
fixchars([]) ->
    [].
