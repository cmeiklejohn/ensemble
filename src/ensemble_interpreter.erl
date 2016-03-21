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
         actor :: binary(),
         variables :: dict:dict()
        }).

%% @doc Evaluate a program.
eval(Program) ->
    %% First, apply lexical analysis.
    {ok, Tokens, _EndLine} = ?LEXER:string(Program),

    %% Next, parse into an AST.
    {ok, ParseTree} = ?PARSER:parse(Tokens),

    %% Generate an actor identifier for execution of this application.
    Actor = term_to_binary(node()),
    lager:info("Generated actor identifier for program: ~p", [Actor]),

    %% Finally, evaluate the program.
    eval(ParseTree, #state{actor=Actor, variables=dict:new()}).

%% @private
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
statement({update, {var, _, Variable}, Expression}, #state{actor=Actor}=State0) ->
    %% Create a new variable.
    {ok, _} = lasp:declare(Variable, ?SET),

    %% Evaluate the expression.
    case expression(Expression, State0) of
        %% We got back the identifier of a variable that contains the
        %% state that we want.
        {{var, _, TheirId}, #state{variables=Variables0}=State1} ->
            lager:info("Original identifier: ~p value: ~p",
                       [Variable, lasp_type:query(?SET, Variable)]),
            lager:info("Received identifier: ~p value: ~p",
                       [TheirId, lasp_type:query(?SET, TheirId)]),

            %% Bind our new variable directly to the shadow variable.
            ok = lasp:bind_to(Variable, TheirId),

            %% Wait for new variable to change from bottom state and
            %% return the result.
            {ok, {_, _, _, V1}} = lasp:read(Variable, {strict, undefined}),
            Value = lasp_type:value(?SET, V1),
            Variables = dict:store(Variable, V1, Variables0),

            %% Return value and updated cache.
            {Value, State1#state{variables=Variables}};

        %% Else, we got back an literal value that we can directly bind
        %% to the variable.
        {Literal, #state{variables=Variables0}=State1} ->
            {ok, {_, _, _, V}} = lasp:update(Variable,
                                            {add_all, Literal},
                                            Actor),

            %% Return current non-CRDT value to the user.
            V1 = lasp_type:value(?SET, V),

            %% Cache last observed value.
            Variables = dict:store(Variable, V, Variables0),
            {V1, State1#state{variables=Variables}}

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
           #state{variables=Variables0}=State0) ->

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
    lager:info("Created shadow variable: ~p", [Destination]),

    %% Execute the map operation.
    ok = lasp:map(Source, Function, Destination),
    lager:info("Current value of source: ~p", [lasp_type:query(?SET, Source)]),

    %% If we perform an operation on a variable, such as a map,
    %% we know the value is going to change.  Therefore, modify
    %% our cache of known values to show that the next read
    %% operation should be strict, to guarantee
    %% read-your-own-writes.
    Previous = case dict:find(Destination, Variables0) of
        {ok, V} ->
            V;
        _ ->
            undefined
    end,
    Variables = dict:store(Destination, {strict, Previous}, Variables0),

    %% Return variable.
    {{var, Line, Destination}, State0#state{variables=Variables}};
expression([Expr|Exprs], State0) ->
    {Value, State} = expression(Expr, State0),
    [Value|expression(Exprs, State)];
expression(V, State) when is_integer(V) ->
    {V, State};
%% Do not evaluate variables any further; it's up to the pretty printer
%% to do that.
expression({var, Line, Variable}, #state{variables=_Variables0}=State0) ->
    {{var, Line, Variable}, State0};
expression({iota, V}, State) ->
    {lists:seq(1, V), State};
expression(Expr, _State) ->
    lager:info("Expression not caught: ~p", [Expr]),
    exit(badarg).

%% @private
pp({var, _, Variable}) ->
    Value = lasp_type:query(?SET, Variable),
    lager:info("Received value: ~p", [Value]),
    pp(Value);
pp(List) when is_list(List) ->
    list_to_binary("{ " ++
                   [io_lib:format("~p ", [Item]) || Item <- List] ++
                   "}").
