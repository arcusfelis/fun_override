-module(fun_override_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("fun_override/include/fun_override.hrl").
-include_lib("fun_override/include/fun_override_prod.hrl").

-fun_override([
    call_me/2,
    unnamed_args_fun/2
]).

-export([call_me/2, unnamed_args_fun/2]).

unnamed_args_fun(_, _B) ->
    5.

call_me(A, B) ->
    A + B.

-ifdef(TEST).

no_override_test() ->
    4 = call_me(1, 3),
    ok.

override_test() ->
    Key = {fun_override, MFA = {?MODULE, call_me, 2}},
    try
        Meta = #{f => fun(Arg) -> Arg end},
        persistent_term:put(Key, Meta),
        #{mfa := MFA, args := [1, 3], orig_fun := OrigFun, meta := Meta} = call_me(1, 3),
        9 = OrigFun(4, 5)
    after
        persistent_term:erase(Key)
    end,
    ok.

assert_disabled_in_prod_test() ->
    ?ASSERT_FUN_OVERRIDE_DISABLED_FOR_MODULES([fun_override_prod]).

assert_disabled_in_dev_test() ->
    ?assertError(
        {fun_override_assert_disabled_failed, [fun_override_tests]},
        ?ASSERT_FUN_OVERRIDE_DISABLED_FOR_MODULES([?MODULE])
    ).

unload_test() ->
    fun_override:expect(?MODULE, call_me, 2, fun(A, B) -> [A, B] end),
    [1, 3] = ?MODULE:call_me(1, 3),
    fun_override:unload(?MODULE),
    4 = ?MODULE:call_me(1, 3).

expect_reload_test() ->
    fun_override:expect(?MODULE, call_me, 2, fun(A, B) -> [A, B] end),
    [1, 3] = ?MODULE:call_me(1, 3),
    fun_override:expect(?MODULE, call_me, 2, x),
    x = ?MODULE:call_me(1, 3),
    fun_override:unload(?MODULE),
    4 = ?MODULE:call_me(1, 3).

mock_unknown_test() ->
    ?assertError(
        {assert_mockable_failed, #{
            mfa := {fun_override_tests, wrong_call_me, 2},
            mockable_functions := [_ | _]
        }},
        fun_override:expect(?MODULE, wrong_call_me, 2, fun(A, B) -> [A, B] end)
    ),
    ?assertError(
        {assert_mockable_failed, #{
            mfa := {fun_override_tests, wrong_call_me, 2},
            mockable_functions := [_ | _]
        }},
        fun_override:expect(?MODULE, wrong_call_me, 2, x)
    ).

mock_unmock_test() ->
    fun_override:mock(?MODULE, call_me, 2, #{f => fun(#{args := [A, B]}) -> [A, B] end}),
    [1, 3] = ?MODULE:call_me(1, 3),
    fun_override:unmock(?MODULE, call_me, 2),
    4 = ?MODULE:call_me(1, 3).

fun_override_not_compiled_test() ->
    ?assertError(
        {module_not_compiled_with_fun_override, fun_override_not_compiled},
        fun_override:mock(fun_override_not_compiled, some_fun, 0, #{f => fun(#{}) -> ok end})
    ).

-endif.
