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

-module(ensemble_interpreter).
-author("Christopher S. Meiklejohn <christopher.meiklejohn@gmail.com>").

-include("ensemble.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([eval/1]).

-record(state, {}).

%% @doc Evaluate a program.
%% @todo Move actor to state.
%% @todo Replace value calls.
%% @todo Move scope to state.
eval(Program) ->
    %% First, apply lexical analysis.
    {ok, Tokens, _EndLine} = ?LEXER:string(Program),

    %% Next, parse into an AST.
    {ok, ParseTree} = ?PARSER:parse(Tokens),

    %% Generate an actor identifier for execution of this application.
    Actor = term_to_binary(node()),
    lager:info("Generated actor identifier for program: ~p",
               [Actor]),
    put(actor, Actor),

    %% Finally, evaluate the program.
    eval(ParseTree, #state{}).

eval([Stmt|[]], State0) ->
    %% Final call, so ignore returned state.
    {Result, _State} = statement(Stmt, State0),
    Result;
eval([Stmt|Stmts], State0) ->
    %% Ignore any intermediate results.
    {_Result, State} = statement(Stmt, State0),
    eval(Stmts, State).

%% @private
statement({update, {var, _, Variable}, Expression}, State) ->
    Actor = get(actor),

    %% Create a new variable.
    {ok, _} = lasp:declare(Variable, ?SET),

    %% Evaluate the expression.
    case expression(Expression, State) of
        %% If the result of the expression needs to spawn a function to
        %% bind a value to our variable, such as for a map and fold;
        %% then return a closure that can be called immediately with the
        %% variable to bind to.
        Function when is_function(Function) ->
            {Function(Variable), State};
        %% Else, if we get a value back, we can directly bind.
        Value ->
            {ok, {_, _, _, V}} = lasp:update(Variable,
                                            {add_all, Value},
                                            Actor),
            put({variable, Variable}, V),
            V1 = ?SET:value(V),
            {V1, State}
    end;
statement({query, {var, _, Variable}}, State) ->
    %% @todo Probably need a new lasp operation for this.
    V = get({variable, Variable}),
    {ok, {_, _, _, Value}} = lasp:read(Variable, V),
    {?SET:value(Value), State};
statement(Stmt, State) ->
    {Stmt, State}.

%% @private
expression({map, {var, _, Var}, {function, {'+', _}}, Val}, _State) ->
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
expression([Expr|Exprs], State) ->
    [expression(Expr, State)|expression(Exprs, State)];
expression(V, _State) when is_integer(V) ->
    lager:info("Integer expression: ~p", [V]),
    V;
expression({iota, V}, _State) ->
    lists:seq(1, V);
expression(Expr, _State) ->
    lager:info("Expression not caught: ~p", [Expr]),
    Expr.
