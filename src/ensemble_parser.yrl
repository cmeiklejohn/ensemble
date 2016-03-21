Nonterminals
statements statement expression int_list function product.

Terminals
'<-' '+' '*' 'i' var integer nl.

Rootsymbol
statements.

%% Expect 1 shift/reduce conflict from the integer list creation.
Expect 1.

statements -> statement : ['$1'].
statements -> statements nl statements : '$1' ++ '$3'.
statements -> statements nl : '$1'.

%% Statements can be of three types: expressions, assignment statements
%% that take expressions, and variables, which are the primary value
%% type.
statement -> var '<-' expression : {update, '$1', '$3'}.

statement -> expression : '$1'.

%% In our language, variables are not expressions, they are values.
%% This is because we will expose too much temporaility in the language
%% if things operate by value, therefore, we reduce to variables.
statement -> var : '$1'.

%% The iota operator performs set generation.
expression -> 'i' integer : {iota, unwrap('$2')}.

%% The map operation applies a function with the rhs argument as the
%% evaluation of the provided expression.
expression -> var function expression : {process, {map, '$1', '$2', '$3'}}.

%% Cartesian product, computes the cross product between two variables.
expression -> var product var : {process, {product, '$1', '$3'}}.

%% Lists of integers, the other primary value type.
expression -> int_list : '$1'.
expression -> integer : unwrap('$1').

int_list -> integer integer : ['$1', '$2'].
int_list -> integer int_list : ['$1'] ++ '$2'.

%% Function types.
function -> '+' : {function, '$1'}.
function -> product : {function, '$1'}.

product -> '*' : '$1'.

Erlang code.

unwrap({_, _, V}) -> V.
