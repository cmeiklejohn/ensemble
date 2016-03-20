Definitions.

D   = [0-9]
V   = [A-Z_][0-9a-zA-Z_]*
L   = [A-Za-z]
CR  = \r
NL  = \n
WS  = ([\000-\s]|%.*)

Rules.

<-                     : {token,{list_to_atom(TokenChars),TokenLine}}.
\+                     : {token,{list_to_atom(TokenChars),TokenLine}}.
i                      : {token,{list_to_atom(TokenChars),TokenLine}}.
\/                     : {token,{list_to_atom(TokenChars),TokenLine}}.
{V}+                   : {token,{var,TokenLine,list_to_atom(TokenChars)}}.
{D}+                   : {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
[(),]                  : {token,{list_to_atom(TokenChars),TokenLine}}.
({CR}|{NL}|{CR}{NL}|;) : {token,{nl,TokenLine}}.
{WS}+                  : skip_token.

Erlang code.
