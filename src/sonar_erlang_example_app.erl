%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2019 Cursor Insight
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% @doc `sonar_erlang_example' application module.
%%% @end
%%%-----------------------------------------------------------------------------

-module(sonar_erlang_example_app).
-include("sonar_erlang_example.hrl").

-behaviour(application).

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% Application callbacks
-export([start/2, stop/1]).

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start the application.
%% @end
%%------------------------------------------------------------------------------
-spec start(StartType, StartArgs) -> {ok, Pid} |
                                     {ok, Pid, State} |
                                     {error, Reason} when
      StartType :: application:start_type(),
      StartArgs :: term(),
      Pid :: pid(),
      State :: term(),
      Reason :: term().
start(_StartType, _StartArgs) ->
    ok = sonar_erlang_example:call_without_spec(),
    ok = sonar_erlang_example:call_without_doc(),
    ok = sonar_erlang_example:try_catch_with_ces(),
    ok = sonar_erlang_example:eunit_covered_function(),
    ok = sonar_erlang_example:ct_covered_function(),
    sonar_erlang_example_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Stop the application.
%% @end
%%------------------------------------------------------------------------------
-spec stop(State) -> ok when
      State :: term().
stop(_State) ->
    ok.
