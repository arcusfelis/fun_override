# Mock library for compilation time

Erlang has Meck library which is useful to mock some functions.
But there are some issues when meck is used together with cover.

One issue is that Meck does not support cover in distributed mode.
Distibuted cover mode is useful when you have a lot of nodes under
test - it safes time by doing cover compilation once.


`fun_override` is a compilation time alternative.

It uses a `parse_transform` which could be enabled by including:

```erlang
-include_lib("fun_override/include/fun_override.hrl").
```

`FUN_OVERRIDE_ENABLED` macro should be set in the rebar.config for development releases:


```erlang
{profiles, [
    {test, [{erl_opts, [{d, 'FUN_OVERRIDE_ENABLED', true}]}]}
]}.
```


If meck tries to alter the module after cover compilation is done,
`fun_override` would try to alter the module before the cover compilation.

`fun_override` rewrites function:

```erlang
-module(fun_example).
-include_lib("fun_override/include/fun_override.hrl").

-fun_override([call_me/2]).

-export([call_me/2]).

call_me(A, B) ->
    A + B.
```

as

```erlang
-module(fun_example).
-include_lib("fun_override/include/fun_override.hrl").

-fun_override([call_me/2]).

-export([call_me/2]).

call_me(V1, V2) ->
    fun_override:call_function(?MODULE, call_me, 2, fun 'call_me_OVERWRITTEN'/2, [V1, V2]).
    
'call_me_OVERWRITTEN'(A, B) ->
    A + B.
```

But it preserves the original line numbers.

`fun_override:call_function/5` does `persistent_term` lookup to check if we should
call a new function or the original one.

# Production

In production the `parse_transform` is off.
You can call `ASSERT_FUN_OVERRIDE_DISABLED_FOR_APPLICATION(YourApplicationName)` to ensure no modules
compiled using the `parse_transform` are loaded.

Something like:

```erlang
-module(your_application).
-include_lib("fun_override/include/fun_override_prod.hrl").

start() ->
   case your_env() of
       dev -> ok;
       prod -> ?ASSERT_FUN_OVERRIDE_DISABLED_FOR_APPLICATION(your_application)
   end,
   ...
```

If there are any modules compiled with the `parse_transform`, the code would crash.
You do not need to include `fun_override` into the production release.
