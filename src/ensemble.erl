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
