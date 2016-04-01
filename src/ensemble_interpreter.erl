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

-record(state,
        {
         actor :: binary()
        }).

%% @doc Evaluate a program.
eval(Program) ->
    %% First, apply lexical analysis.
    {ok, Tokens, _EndLine} = ?LEXER:string(Program),

    %% Next, parse into an AST.
    {ok, ParseTree} = ?PARSER:parse(Tokens),

    %% Generate an actor identifier for execution of this application.
    Actor = term_to_binary(node()),

    %% Finally, evaluate the program.
    eval(ParseTree, #state{actor=Actor}).

%% @private
eval('$undefined', _State) ->
    %% If the parse tree produced nothing, evaluation returns the empty
    %% set, I think?
    pp([]);
eval([Stmt|[]], State0) ->
    %% Final call, so ignore returned state.
    {Result, _State} = statement(Stmt, State0),
    pp(Result);
eval([Stmt|Stmts], State0) ->
    %% Ignore any intermediate results.
    {_Result, State} = statement(Stmt, State0),
    eval(Stmts, State).

%% @private
%%
%% Update is an assignment statement, where the expression on the rhs is
%% evaluated and assigned to the variable on the lhs; the variable on
%% the lhs may not exist, so the variable needs to first be delcared.
%%
statement({update, {var, Line, Variable}, Expression},
          #state{actor=Actor}=State0) ->
    %% Create a new variable.
    {ok, _} = lasp:declare(Variable, ?SET),

    %% Evaluate the expression.
    case expression(Expression, State0) of
        %% We got back the identifier of a variable that contains the
        %% state that we want.
        {{var, _, TheirId}, State1} ->

            %% Bind our new variable directly to the shadow variable.
            ok = lasp:bind_to(Variable, TheirId),

            %% Return variable.
            {{var, Line, Variable}, State1};

        %% Else, we got back an literal value that we can directly bind
        %% to the variable.
        {Literal, State1} ->

            %% Bind updated value.
            {ok, _} = lasp:update(Variable, {add_all, Literal}, Actor),

            %% Return variable.
            {{var, Line, Variable}, State1}

    end;
%% Otherwise, the statement must be an expression that evaluates to a
%% value that will be returned to the user.  Attempt to evaluate this
%% expression.
statement(Stmt, State) ->
    expression(Stmt, State).

%% @private
%%
%% When a process call is received, create a shadow variable to store
%% the results of the map operation and return the identifier to the
%% caller of map: it's their decision whether to query and return the
%% result to the user, or assign the variable to another variable in the
%% system.
%%
expression({process,
            {map, {var, Line, Source}, {function, {Function0, _}}, Val}},
           State0) ->

    %% Generate an Erlang anonymous function.
    Function = case Function0 of
        '+' ->
            fun(X) -> X + Val end;
        '*' ->
            fun(X) -> X * Val end
    end,

    %% Create a shadow variable used to store the result of the fold
    %% operation; this will get an anonymous global variable.
    %%
    {ok, {Destination, _, _, _}} = lasp:declare(?SET),

    %% Execute the map operation.
    ok = lasp:map(Source, Function, Destination),

    %% Return variable.
    {{var, Line, Destination}, State0};
expression({process,
            {union, {var, Line, Left}, {var, Line, Right}}},
           State0) ->

    %% Create a shadow variable used to store the result of the
    %% operation; this will get an anonymous global variable.
    %%
    {ok, {Union, _, _, _}} = lasp:declare(?SET),

    %% Execute the operation.
    ok = lasp:union(Left, Right, Union),

    %% Return variable.
    {{var, Line, Union}, State0};
expression({process,
            {intersection, {var, Line, Left}, {var, Line, Right}}},
           State0) ->

    %% Create a shadow variable used to store the result of the
    %% operation; this will get an anonymous global variable.
    %%
    {ok, {Intersection, _, _, _}} = lasp:declare(?SET),

    %% Execute the operation.
    ok = lasp:intersection(Left, Right, Intersection),

    %% Return variable.
    {{var, Line, Intersection}, State0};
expression({process,
            {product, {var, Line, Left}, {var, Line, Right}}},
           State0) ->

    %% Create a shadow variable used to store the result of the fold
    %% operation; this will get an anonymous global variable.
    %%
    {ok, {Product, _, _, _}} = lasp:declare(?SET),

    %% Execute the operation.
    ok = lasp:product(Left, Right, Product),

    %% Return variable.
    {{var, Line, Product}, State0};
expression(List0, State0) when is_list(List0) ->
    lists:foldl(fun(Expr, {List, State}) ->
                        {V, S} = expression(Expr, State),
                        {List ++ [V], S}
                end, {[], State0}, List0);
expression(V, State) when is_integer(V) ->
    {V, State};
%% Do not evaluate variables any further; it's up to the pretty printer
%% to do that.
expression({var, Line, Variable}, State0) ->
    {{var, Line, Variable}, State0};
expression({iota, V}, State) ->
    {lists:seq(1, V), State};
expression(Expr, _State) ->
    lager:info("Expression not caught: ~p", [Expr]),
    exit(badarg).

%% @private
%% @todo: Block for at least the initial transition off of the bottom.
pp({var, _, Variable}) ->
    case lasp:read(Variable, {strict, undefined}) of
        {ok, {_, _, _, Value0}} ->
            Value = lasp_type:value(?SET, Value0),
            pp(Value);
        {error, not_found} ->
            io:format("Variable ~p is not in scope.~n", [Variable]),
            exit(badarg)
    end;
pp(List) when is_list(List) ->
    list_to_binary("{ " ++ [pp(Item) || Item <- List] ++ "}");
pp({A, B}) ->
    io_lib:format("(~p, ~p) ", [A, B]);
pp(X) ->
    io_lib:format("~p ", [X]).
