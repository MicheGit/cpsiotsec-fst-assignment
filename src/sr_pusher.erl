-module(sr_pusher).
-export([push_candy/2, pusher/1]).

push_candy(PusherPid, {candy, _} = Candy) ->
    PusherPid ! Candy.

pusher(Sink) ->
    receive
        {exclude, Flavor} -> 
            receive
                {candy, Flavor} ->
                    Sink ! {reject, Flavor};
                {candy, _} ->
                    Sink ! {accept, Flavor}
            end;
        _ ->
            receive
                {candy, Flavor} ->
                    Sink ! {accept, Flavor}
            end
    end,
    pusher(Sink).