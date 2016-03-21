Nonterminals
statements statement expression int_list function.

Terminals
'<-' '+' '*' '/' 'i' var integer nl.

Rootsymbol
statements.

%% Expect 1 shift/reduce conflict from the integer list creation.
Expect 1.

statements -> statement : ['$1'].
statements -> statements nl statements : '$1' ++ '$3'.
statements -> statements nl : '$1'.

statement -> var '<-' expression : {update, '$1', '$3'}.
statement -> expression : '$1'.

expression -> 'i' integer : {iota, unwrap('$2')}.
expression -> var function expression : {process, {map, '$1', '$2', '$3'}}.
expression -> function '/' expression : {process, {foldr, '$1', '$3'}}.
expression -> var : {query, '$1'}.
expression -> int_list : '$1'.
expression -> integer : unwrap('$1').

function -> '+' : {function, '$1'}.
function -> '*' : {function, '$1'}.

int_list -> integer integer : ['$1', '$2'].
int_list -> integer int_list : ['$1'] ++ '$2'.

Erlang code.

unwrap({_, _, V}) -> V.
