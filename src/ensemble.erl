%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Christopher S. Meiklejohn.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(ensemble).
-author("Christopher S. Meiklejohn <christopher.meiklejohn@gmail.com>").

-include("ensemble.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([eval/1]).

%% @doc Evaluate a program.
eval([Stmt|[]]) ->
    %% Use the node name as the actor identifier, for now.
    put(actor, term_to_binary(node())),

    statement(Stmt);
eval([Stmt|Stmts]) ->
    statement(Stmt),
    eval(Stmts).

%% @private
statement({update, {var, _, Variable}, Expression}) ->
    Actor = get(actor),

    %% Create a new variable.
    {ok, _} = lasp:declare(Variable, ?SET),

    %% Evaluate the expression.
    case expression(Expression) of
        %% If the result of the expression needs to spawn a function to
        %% bind a value to our variable, such as for a map and fold;
        %% then return a closure that can be called immediately with the
        %% variable to bind to.
        Function when is_function(Function) ->
            Function(Variable);
        %% Else, if we get a value back, we can directly bind.
        Value ->
            {ok, {_, _, _, V}} = lasp:update(Variable,
                                            {add_all, Value},
                                            Actor),
            put({variable, Variable}, V),
            V
    end;
statement({query, {var, _, Variable}}) ->
    %% @todo Probably need a new lasp operation for this.
    V = get({variable, Variable}),
    {ok, {_, _, _, Value}} = lasp:read(Variable, V),
    ?SET:value(Value);
statement(Stmt) ->
    Stmt.

%% @private
expression({map, {var, _, Var}, {function, {'+', _}}, {integer, _, Val}}) ->
    fun(Variable) ->

            %% Produce a closure that will be executed for the map
            %% operation.
            ok = lasp:map(Var, fun(X) -> X + Val end, Variable),

            %% If we perform an operation on a variable, such as a map,
            %% we know the value is going to change.  Therefore, modify
            %% our cache of known values to show that the next read
            %% operation should be strict, to guarantee
            %% read-your-own-writes.
            V = get({variable, Variable}),
            put({variable, Variable}, {strict, V})

    end;
expression([Expr|Exprs]) ->
    [expression(Expr)|expression(Exprs)];
expression({integer, _, I}) ->
    I;
expression(Expr) ->
    Expr.

-ifdef(TEST).

%% @doc Ensure we can parse assignments.
assignment_test() ->
    {ok, Tokens, _EndLine} = ?LEXER:string("A <- 1 2 3 4"),
    LexerExpected = [{var,1,'A'},
                     {'<-',1},
                     {integer,1,1},
                     {integer,1,2},
                     {integer,1,3},
                     {integer,1,4}],
    ?assertMatch(LexerExpected, Tokens),
    {ok, ParseTree} = ?PARSER:parse(Tokens),
    ParserExpected = [{update,{var,1,'A'},[{integer,1,1},
                                           {integer,1,2},
                                           {integer,1,3},
                                           {integer,1,4}]}],
    ?assertMatch(ParserExpected, ParseTree).

%% @doc Ensure we can parse print variables.
print_test() ->
    {ok, Tokens, _EndLine} = ?LEXER:string("A"),
    LexerExpected = [{var,1,'A'}],
    ?assertMatch(LexerExpected, Tokens),
    {ok, ParseTree} = ?PARSER:parse(Tokens),
    ParserExpected = [{query,{var,1,'A'}}],
    ?assertMatch(ParserExpected, ParseTree).

%% @doc Ensure we can parse map operations.
map_test() ->
    {ok, Tokens, _EndLine} = ?LEXER:string("A+1"),
    LexerExpected = [{var,1,'A'},{'+',1},{integer,1,1}],
    ?assertMatch(LexerExpected, Tokens),
    {ok, ParseTree} = ?PARSER:parse(Tokens),
    ParserExpected = [{map,{var,1,'A'},{function,{'+',1}},{integer,1,1}}],
    ?assertMatch(ParserExpected, ParseTree).

%% @doc Ensure we can parse over operations.
over_test() ->
    {ok, Tokens, _EndLine} = ensemble_lexer:string("+/A"),
    LexerExpected = [{'+',1},{'/',1},{var,1,'A'}],
    ?assertMatch(LexerExpected, Tokens),
    {ok, ParseTree} = ?PARSER:parse(Tokens),
    ParserExpected = [{foldr,{function,{'+',1}},{query,{var,1,'A'}}}],
    ?assertMatch(ParserExpected, ParseTree).

-endif.
