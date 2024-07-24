-module(fun_override).

-export([call_function/5]).

%% We do not include any unsafe functions into the production release
-export([expect/4, mock/4, unmock/3, unload/1]).

call_function(Module, FunName, Arity, OrigFun, Args) ->
    MFA = {Module, FunName, Arity},
    case persistent_term:get({fun_override, MFA}, false) of
        false ->
            apply(OrigFun, Args);
        #{f := F} = Meta ->
            F(#{mfa => MFA, args => Args, orig_fun => OrigFun, meta => Meta})
    end.

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

