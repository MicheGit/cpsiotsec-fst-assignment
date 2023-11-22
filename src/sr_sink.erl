-module(sr_sink).
-export([sink/0]).

sink() ->
    receive
        A -> logger:notice(lists:flatten(io_lib:format("~p",[A])))
    end,
    sink().