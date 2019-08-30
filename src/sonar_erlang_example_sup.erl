%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2019 Cursor Insight
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% @doc `sonar_erlang_example' top level supervisor.
%%% @end
%%%-----------------------------------------------------------------------------

-module(sonar_erlang_example_sup).
-include("sonar_erlang_example.hrl").

-behaviour(supervisor).

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% API
-export([start_link/0]).

%% Supervisor callback
-export([init/1]).

%%%=============================================================================
%%% Macros
%%%=============================================================================

-define(TIMEOUT, 5000). % 5 seconds

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start the supervisor
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, Pid} | ignore | {error, Reason} when
      Pid :: pid(),
      Reason :: {already_started, pid()} | {shutdown, term()} | term().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Config = #{}).

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Return the supervisor's initial configuration.
%% @end
%%------------------------------------------------------------------------------
-spec init(Config) -> {ok, {SupFlags, ChildSpecs}} | ignore when
      Config :: #{},
      SupFlags :: supervisor:sup_flags(),
      ChildSpecs :: [supervisor:child_spec()].
init(_Config = #{}) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 3,
                 period => 1},
    ChildSpecs = [%#{id => XXX_child_module_XXX,
                  %  start => {XXX_child_module_XXX, start_link, []},
                  %  restart => permanent,
                  %  shutdown => 1000,
                  %  type => supervisor}
                  ],
    {ok, {SupFlags, ChildSpecs}}.
