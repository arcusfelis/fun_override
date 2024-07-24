-module(fun_override_tests).

-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, fun_override_parse_transform}).
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

-endif.
