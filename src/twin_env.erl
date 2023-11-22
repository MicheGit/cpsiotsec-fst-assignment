-module(twin_env).
-export([spawn_twin/4]).


replicate(Main, Twin) ->
    receive
        A -> Main ! A, Twin ! A
    end,
    replicate(Main, Twin).

spawn_twin(Module, ProcessFn, Args, TwinArgs) ->
    {Main, _} = spawn_monitor(Module, ProcessFn, Args),
    {Twin, _} = spawn_monitor(Module, ProcessFn, TwinArgs),
    {Repl, _} = spawn_monitor(fun() -> replicate(Main, Twin) end),
    {Repl, Twin}.

