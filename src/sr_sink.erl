-module(sr_sink).
-export([sink/0]).

sink() ->
    receive
        A -> logger:notice(A)
    end,
    sink().