%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2019 Cursor Insight
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------

-module(sonar_erlang_example).
-include("sonar_erlang_example.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

-export([call_without_spec/0,
         call_without_doc/0,
         try_catch_with_ces/0,
         call__With_wrong_name/0,
         eunit_covered_function/0,
         ct_covered_function/0,
         not_used_call/0]).

%%%=============================================================================
%%% API implementations
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Function without spec
%% @end
%%------------------------------------------------------------------------------
call_without_spec() ->
    ok.

-spec call_without_doc() -> ok.
call_without_doc() ->
    ok.

%%------------------------------------------------------------------------------
%% @doc Function with try-catch consisting `Class:Exception:Stacktrace' style
%%      exception matching.
%% @end
%%------------------------------------------------------------------------------
-spec try_catch_with_ces() -> ok.
try_catch_with_ces() ->
    try
        call_without_spec()
    of
        ok ->
            ok
    catch
        _C:_E:_S ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc Function with wrong naming conventions.
%% @end
%%------------------------------------------------------------------------------
-spec call__With_wrong_name() -> ok.
call__With_wrong_name() ->
    _variableWithWrongName = 5,
    ok.

%%------------------------------------------------------------------------------
%% @doc Eunit covered function
%% @end
%%------------------------------------------------------------------------------
-spec eunit_covered_function() -> ok.
eunit_covered_function() ->
    ok.

%%------------------------------------------------------------------------------
%% @doc CT covered function
%% @end
%%------------------------------------------------------------------------------
-spec ct_covered_function() -> ok.
ct_covered_function() ->
    ok.

%%------------------------------------------------------------------------------
%% @doc Not used function
%% @end
%%------------------------------------------------------------------------------
-spec not_used_call() -> ok.
not_used_call() ->
    ok.

%%%=============================================================================
%%% Eunit tests
%%%=============================================================================

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

truth_test() ->
    ?assert(true).

cover_test() ->
    ?assertEqual(ok, eunit_covered_function()).

-endif.
