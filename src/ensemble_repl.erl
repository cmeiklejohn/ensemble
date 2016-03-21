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

-module(ensemble_repl).
-author("Christopher S. Meiklejohn <christopher.meiklejohn@gmail.com>").

-export([start/0]).

-include("ensemble.hrl").

-record(state, {counter :: non_neg_integer()}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Initialize a repl.
-spec start() -> no_return().
start() ->
    State = #state{counter=1},
    loop(State).

%% @private
loop(#state{counter=Counter}=State) ->

    %% Generate a prompt with the current line number.
    Prompt = io_lib:format("Ensemble(~p)> ", [Counter]),

    %% Get input from the user.
    Data = io:get_line(Prompt),

    try
        %% Evaulate received input with the interpreter.
        Result = ?INTERPRETER:eval(Data),

        %% Output the result.
        io:format("~s~n", [binary_to_list(Result)])
    catch
        _:Error ->
            io:format("~n", []),
            io:format("Error: ~p~n", [Error]),
            io:format("~n", []),
            io:format("Backtrace:~n~p~n", [erlang:get_stacktrace()]),
            io:format("~n", [])
    end,

    loop(State#state{counter=Counter+1}).
