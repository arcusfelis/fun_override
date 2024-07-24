-module(fun_override).

-export([assert_disabled/1, assert_disabled_modules/1]).
-export([call_function/5]).

%% We do not include any unsafe functions into the production release
-ifdef(FUN_OVERRIDE_ENABLED).
-export([expect/4, mock/4, unmock/3, unload/1]).
-endif.


%% @doc Run this function before starting your application
%% in production to ensure there are no modules with fun_override compiled.
assert_disabled(Application) ->
    {ok, Mods} = application:get_key(Application, modules),
    assert_disabled_modules(Mods).

assert_disabled_modules(Mods) ->
    Enabled = [Mod || Mod <- Mods, is_enabled(Mod)],
    case Enabled of
        [] ->
            ok;
        [_ | _] ->
            error_logger:error_msg("fun_override is enabled for modules ~p", [Enabled]),
            error({fun_override_assert_disabled_failed, Enabled})
    end.

is_enabled(Mod) ->
    proplists:get_value(fun_override_enabled, Mod:module_info(attributes)) =:= [ok].

-ifdef(FUN_OVERRIDE_ENABLED).
call_function(Module, FunName, Arity, OrigFun, Args) ->
    MFA = {Module, FunName, Arity},
    case persistent_term:get({fun_override, MFA}, false) of
        false ->
            apply(OrigFun, Args);
        #{f := F} = Meta ->
            F(#{mfa => MFA, args => Args, orig_fun => OrigFun, meta => Meta})
    end.
-else.
call_function(_Module, _FunName, _Arity, OrigFun, Args) ->
    apply(OrigFun, Args).
-endif.


-ifdef(FUN_OVERRIDE_ENABLED).

%% @doc Similar API for mock.
expect(M, FN, A, F) when is_function(F) ->
    Meta = #{f => fun(#{args := Args}) -> apply(F, Args) end},
    mock(M, FN, A, Meta);
expect(M, FN, A, V) ->
    Meta = #{f => fun(#{}) -> V end},
    mock(M, FN, A, Meta).

mock(M, FN, A, Meta) ->
    assert_mockable(M, FN, A),
    Key = {fun_override, {M, FN, A}},
    persistent_term:put(Key, Meta).

unmock(M, FN, A) ->
    Key = {fun_override, {M, FN, A}},
    persistent_term:erase(Key).

assert_mockable(Mod, FN, A) ->
    FAs = fas(Mod),
    FA = {FN, A},
    case lists:member(FA, FAs) of
        true ->
            ok;
        false ->
            error({assert_mockable_failed, #{mfa => {Mod, FN, A}, mockable_functions => FAs}})
    end.

unload(Mod) ->
    [persistent_term:erase({fun_override, MFA}) || MFA <- mfas(Mod)].

mfas(Mod) ->
    [fa_to_mfa(Mod, FA) || FA <- fas(Mod)].

fas(Mod) ->
    proplists:get_value(fun_override, Mod:module_info(attributes), []).

fa_to_mfa(Mod, {FN, A}) ->
    {Mod, FN, A}.

-endif.
