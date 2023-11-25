-module(sr_sink).
-export([sink/0]).

sink() ->
    receive
        A -> logger:warning("[SINK PROC] ~p",[A])
    end,
    sink().