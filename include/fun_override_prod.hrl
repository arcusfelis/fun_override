-define(ASSERT_FUN_OVERRIDE_DISABLED_FOR_APPLICATION(Application),
    begin
        {ok, Mods} = application:get_key(Application, modules),
        ?ASSERT_FUN_OVERRIDE_DISABLED_FOR_MODULES(Mods)
    end).

-define(ASSERT_FUN_OVERRIDE_DISABLED_FOR_MODULES(Mods),
    case [Mod || Mod <- Mods, ?IS_FUN_OVERRIDE_ENABLED_FOR_MODULE(Mod)] of
        [] ->
            ok;
        [_ | _] = Enabled ->
            error_logger:error_msg("fun_override is enabled for modules ~p", [Enabled]),
            error({fun_override_assert_disabled_failed, Enabled})
    end).

-define(IS_FUN_OVERRIDE_ENABLED_FOR_MODULE(Mod),
    begin proplists:get_value(fun_override_enabled, Mod:module_info(attributes)) =:= [ok] end).
