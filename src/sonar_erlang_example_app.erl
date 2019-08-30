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
    sonar_erlang_example_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Stop the application.
%% @end
%%------------------------------------------------------------------------------
-spec stop(State) -> ok when
      State :: term().
stop(_State) ->
    ok.
