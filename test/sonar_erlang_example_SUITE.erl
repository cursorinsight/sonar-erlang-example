%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2019 Cursor Insight
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% `sonar_erlang_example' test suite.
%%%-----------------------------------------------------------------------------

-module(sonar_erlang_example_SUITE).
-include("sonar_erlang_example.hrl").

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

-compile(export_all).
-compile(nowarn_export_all).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type config() :: [tuple()].
-type test_case() :: atom().

%%%=============================================================================
%%% CT callback
%%%=============================================================================

-spec all() -> Result when
      Result :: [test_case()].
all() ->
    [basic_test_TEST,
     cover_TEST].

%%%-----------------------------------------------------------------------------
%%% Test suite init/end
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Initialize before the test suite.
%% @end
%%------------------------------------------------------------------------------
-spec init_per_suite(Config) -> Config when
      Config :: config().
init_per_suite(Config) ->
    Config.

%%------------------------------------------------------------------------------
%% @doc Clean up after the test suite.
%% @end
%%------------------------------------------------------------------------------
-spec end_per_suite(Config) -> ok when
      Config :: config().
end_per_suite(_Config) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% Test case init/end
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Initialize before a test case.
%% @end
%%------------------------------------------------------------------------------
-spec init_per_testcase(Testcase, Config) -> Config when
      Testcase :: test_case(),
      Config :: config().
init_per_testcase(_Testcase, Config) ->
    Config.

%%------------------------------------------------------------------------------
%% @doc Clean up after a test case.
%% @end
%%------------------------------------------------------------------------------
-spec end_per_testcase(Testcase, Config) -> ok when
      Testcase :: test_case(),
      Config :: config().
end_per_testcase(_Testcase, _Config)->
    ok.

%%%=============================================================================
%%% Test cases
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Empty test case.
%% @end
%%------------------------------------------------------------------------------
-spec basic_test_TEST(Config) -> ok when
      Config :: config().
basic_test_TEST(_Config) ->
    ?assertEqual(ok, ok),
    ok.

%%------------------------------------------------------------------------------
%% @doc Simple test case providing code coverage only.
%% @end
%%------------------------------------------------------------------------------
-spec cover_TEST(Config) -> ok when
      Config :: config().
cover_TEST(_Config) ->
    ?assertEqual(ok, sonar_erlang_example:ct_covered_function()),
    ok.

%%%=============================================================================
%%% Helper functions
%%%=============================================================================
