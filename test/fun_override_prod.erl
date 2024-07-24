-module(fun_override_prod).

-undef(FUN_OVERRIDE_ENABLED).
-include_lib("fun_override/include/fun_override.hrl").

-fun_override([
    call_me/2
]).

-export([call_me/2]).

call_me(A, B) ->
    A + B.
