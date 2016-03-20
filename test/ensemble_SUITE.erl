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
%%

-module(ensemble_SUITE).
-author("Christopher S. Meiklejohn <christopher.meiklejohn@gmail.com>").

%% common_test callbacks
-export([%% suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0]).

%% tests
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/inet.hrl").

%% ===================================================================
%% common_test callbacks
%% ===================================================================

init_per_suite(_Config) ->
    application:ensure_all_started(lasp),
    _Config.

end_per_suite(_Config) ->
    application:stop(lasp),
    _Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

all() ->
    [eval_test,
     iota_0_test].

%% ===================================================================
%% tests
%% ===================================================================

-include("ensemble.hrl").

%% @doc Parse a full program
eval_test(_Config) ->
    Filename = code:priv_dir(ensemble) ++ "/test.ens",
    {ok, Binary} = file:read_file(Filename),
    ct:pal("Read file: ~p", [Binary]),
    List = binary_to_list(Binary),
    {ok, Tokens, _EndLine} = ?LEXER:string(List),
    ct:pal("Tokenized file: ~p", [Tokens]),
    {ok, ParseTree} = ?PARSER:parse(Tokens),
    ct:pal("Parse tree: ~p", [ParseTree]),
    ?assertMatch([2,3,4,5], ensemble_interpreter:eval(ParseTree)).

%% @doc Verify the iota behaviour.
iota_0_test(_Config) ->
    {ok, Tokens, _EndLine} = ?LEXER:string("A <- i10\nA"),
    {ok, ParseTree} = ?PARSER:parse(Tokens),
    ?assertMatch([0,1,2,3,4,5,6,7,8,9,10], ensemble_interpreter:eval(ParseTree)).
