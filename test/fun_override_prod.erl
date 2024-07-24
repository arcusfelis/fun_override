-module(fun_override_prod).

-define(PROD_NODE, true).
-include_lib("fun_override/include/fun_override.hrl").

-fun_override([
    call_me/2
  ]).

-export([call_me/2]).

call_me(A, B) ->
    A + B.
