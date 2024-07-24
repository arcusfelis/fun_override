-ifndef(FUN_OVERRIDE_ENABLED).
-define(FUN_OVERRIDE_ENABLED, false).
-endif.

-if(?FUN_OVERRIDE_ENABLED).
-compile({parse_transform, fun_override_parse_transform}).
-fun_override_enabled(ok).
-endif.
