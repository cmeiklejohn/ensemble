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
    lasp:reset(),
    Config.

end_per_testcase(_, _Config) ->
    ok.

all() ->
    [
     iota_test,
     map_plus_test,
     map_times_test,
     map_without_assignment_test,
     product_without_assignment_test,
     intersection_without_assignment_test,
     union_without_assignment_test,
     fold_plus_without_assignment_test,
     assignment_test,
     empty_test
    ].

%% ===================================================================
%% tests
%% ===================================================================

-include("ensemble.hrl").

%% @doc Verify the iota behaviour.
iota_test(_Config) ->
    Program = io_lib:format("A <- i10", []),
    <<"{ 1 2 3 4 5 6 7 8 9 10 }">> = ?INTERPRETER:eval(Program),
    Program2 = io_lib:format("A <- i10; A", []),
    <<"{ 1 2 3 4 5 6 7 8 9 10 }">> = ?INTERPRETER:eval(Program2),
    ok.

%% @doc Verify the map behaviour.
map_plus_test(_Config) ->
    Program = io_lib:format("A <- i10; B <- A + 1; B", []),
    <<"{ 2 3 4 5 6 7 8 9 10 11 }">> = ?INTERPRETER:eval(Program),
    ok.

%% @doc Verify the map behaviour.
map_times_test(_Config) ->
    Program2 = io_lib:format("A <- i5; B <- A * 2; B", []),
    <<"{ 2 4 6 8 10 }">> = ?INTERPRETER:eval(Program2),
    ok.

%% @doc Verify the map behaviour.
map_without_assignment_test(_Config) ->
    Program2 = io_lib:format("A <- i5; A * 2", []),
    <<"{ 2 4 6 8 10 }">> = ?INTERPRETER:eval(Program2),
    ok.

%% @doc Verify the product behaviour.
product_without_assignment_test(_Config) ->
    Program2 = io_lib:format("A <- i2; B <- i2; A x B", []),
    <<"{ (1, 1) (1, 2) (2, 1) (2, 2) }">> = ?INTERPRETER:eval(Program2),
    ok.

%% @doc Verify the intersection behaviour.
intersection_without_assignment_test(_Config) ->
    Program2 = io_lib:format("A <- i2; B <- i2; A & B", []),
    <<"{ 1 2 }">> = ?INTERPRETER:eval(Program2),
    ok.

%% @doc Verify the union behaviour.
union_without_assignment_test(_Config) ->
    Program2 = io_lib:format("A <- i2; B <- i2; A | B", []),
    <<"{ 1 2 }">> = ?INTERPRETER:eval(Program2),
    ok.

%% @doc Verify the fold behaviour.
fold_plus_without_assignment_test(_Config) ->
    Program2 = io_lib:format("A <- i5; +/A", []),
    <<"15">> = ?INTERPRETER:eval(Program2),
    ok.

%% @doc Verify the assignment behaviour.
assignment_test(_Config) ->
    Program = io_lib:format("A <- 1 2 3 4", []),
    <<"{ 1 2 3 4 }">> = ?INTERPRETER:eval(Program),
    ok.

%% @doc Verify the empty behaviour.
empty_test(_Config) ->
    Program = io_lib:format("\n", []),
    <<"{ }">> = ?INTERPRETER:eval(Program),
    ok.

%% @todo Product, fold, intersection, and union all need to support
%%       literals.
