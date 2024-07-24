-module(fun_override).

-export([assert_disabled/1, assert_disabled_modules/1]).
-export([parse_transform/2]).
-export([call_function/5]).

-export([expect/4, unload/1]).

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
        [_|_] ->
            error_logger:error_msg("fun_override is enabled for modules ~p", [Enabled]),
            error({fun_override_assert_disabled_failed, Enabled})
    end.

is_enabled(Mod) ->
    proplists:get_value(fun_override_enabled, Mod:module_info(attributes)) =:= [ok].

expect(M, FN, A, F) when is_function(F) ->
    assert_mockable(M, FN, A),
    Key = {fun_override, {M, FN, A}},
    Meta = #{f => fun(#{args := Args}) -> apply(F, Args) end},
    persistent_term:put(Key, Meta);
expect(M, FN, A, V) ->
    assert_mockable(M, FN, A),
    Key = {fun_override, {M, FN, A}},
    Meta = #{f => fun(#{}) -> V end},
    persistent_term:put(Key, Meta).

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

call_function(Module, FunName, Arity, OrigFun, Args) ->
    MFA = {Module, FunName, Arity},
    case persistent_term:get({fun_override, MFA}, false) of
        false ->
            apply(OrigFun, Args);
        #{f := F} = Meta ->
            F(#{mfa => MFA, args => Args, orig_fun => OrigFun, meta => Meta})
    end.

parse_transform(Forms, _Options) ->
    Attrs = [Form || Form <- Forms, get_attr_name(Form) =:= fun_override],
    Args = [erl_syntax:attribute_arguments(Attr) || Attr <- Attrs],
    [{module, Module}|_] = [erl_syntax_lib:analyze_attribute(Form) || Form <- Forms, get_attr_name(Form) =:= module],
    FAs = lists:append([fun_override_attr_to_fa(Arg) || Arg <- Args]),
    case FAs of
        [] ->
            Forms;
        _ ->
            lists:append([rewrite_functions(Form, FAs, Module) || Form <- Forms])
    end.

get_attr_name(Form) ->
    case erl_syntax:type(Form) of
        attribute ->
            NameForm = erl_syntax:attribute_name(Form),
            case erl_syntax:type(NameForm) of
                atom ->
                    erl_syntax:atom_value(NameForm);
                _ ->
                    error
            end;
        _ ->
            error
    end.

fun_override_attr_to_fa([Form|_]) ->
    case erl_syntax:type(Form) of
        list ->
            % -fun_override([my_fun/2, other_fun/4])
            Els = erl_syntax:list_elements(Form),
            [fun_override_attr_to_fa1(El) || El <- Els];
        tuple ->
            % -fun_override(my_fun/2)
            [fun_override_attr_to_fa1(Form)]
    end.

-spec fun_override_attr_to_fa1(erl_syntax:syntaxTree()) -> {FunName :: atom(), Arity :: integer()}.
fun_override_attr_to_fa1(Form) ->
    case erl_syntax:type(Form) of
        tuple ->
            case erl_syntax:tuple_elements(Form) of
                [Name, Arity] ->
                    case {erl_syntax:type(Name), erl_syntax:type(Arity)} of
                        {atom, integer} ->
                            {erl_syntax:atom_value(Name), erl_syntax:integer_value(Arity)};
                        _ ->
                            error({fun_override_attr_to_fa1_failed, Form})
                    end;
                _ ->
                    error({fun_override_attr_to_fa1_failed, Form})
            end;
        Other ->
            error({fun_override_attr_to_fa1_failed, Form, Other})
    end.

rewrite_functions(Form, FAs, Module) ->
    case erl_syntax:type(Form) of
        function ->
            FA = erl_syntax_lib:analyze_function(Form),
            case lists:member(FA, FAs) of
                true ->
                    rewrite_function(Form, FA, Module);
                false ->
                    [Form]
            end;
        _ ->
            [Form]
    end.

rewrite_function(Form, {AtomName, Arity}, Module) ->
    NameForm = erl_syntax:function_name(Form),
    AtomName2 = list_to_atom(atom_to_list(AtomName) ++ "_OVERWRITTEN"),
    NameForm2 = erl_syntax:set_pos(erl_syntax:atom(AtomName2), erl_syntax:get_pos(NameForm)),
    Clauses = erl_syntax:function_clauses(Form),
    Overwritten = erl_syntax:function(NameForm2, Clauses),
    NewFun = make_proxy_fun(AtomName, AtomName2, Arity, Module),
    [erl_syntax:revert(Overwritten), erl_syntax:revert(NewFun)].

make_proxy_fun(AtomName, AtomName2, Arity, Module) ->
    Fun = erl_syntax:implicit_fun(none, erl_syntax:atom(AtomName2), erl_syntax:integer(Arity)),
    Args = nth_variables(Arity),
    CallArgs = [erl_syntax:atom(Module), erl_syntax:atom(AtomName), erl_syntax:integer(Arity), Fun, erl_syntax:list(Args)],
    Call = erl_syntax:application(erl_syntax:atom(?MODULE), erl_syntax:atom(call_function), CallArgs),
%   Body = [erl_syntax:application(erl_syntax:atom(AtomName2), Args)],
    Body = [Call],
    Clause = erl_syntax:clause(Args, [], Body),
    erl_syntax:function(erl_syntax:atom(AtomName), [Clause]).

nth_variables(Arity) ->
    [erl_syntax:variable("V" ++ integer_to_list(N)) || N <- lists:seq(1, Arity)].
