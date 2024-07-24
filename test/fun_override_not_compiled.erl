-module(fun_override_not_compiled).
-export([some_fun/0]).

-fun_overide([some_fun/0]).

some_fun() -> ok.
