-module(fun_override_parse_transform).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    Attrs = [Form || Form <- Forms, get_attr_name(Form) =:= fun_override],
    Args = [erl_syntax:attribute_arguments(Attr) || Attr <- Attrs],
    [{module, Module} | _] = [
        erl_syntax_lib:analyze_attribute(Form)
     || Form <- Forms, get_attr_name(Form) =:= module
    ],
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

fun_override_attr_to_fa([Form | _]) ->
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
    CallArgs = [
        erl_syntax:atom(Module),
        erl_syntax:atom(AtomName),
        erl_syntax:integer(Arity),
        Fun,
        erl_syntax:list(Args)
    ],
    Call = erl_syntax:application(
        erl_syntax:atom(fun_override), erl_syntax:atom(call_function), CallArgs
    ),
    Body = [Call],
    Clause = erl_syntax:clause(Args, [], Body),
    erl_syntax:function(erl_syntax:atom(AtomName), [Clause]).

nth_variables(Arity) ->
    [erl_syntax:variable("V" ++ integer_to_list(N)) || N <- lists:seq(1, Arity)].
