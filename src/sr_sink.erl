-module(sr_sink).
-export([sink/0]).

sink() ->
    receive
        A -> logger:notice("[CANDY SINK] ~p",[A])
    end,
    sink().