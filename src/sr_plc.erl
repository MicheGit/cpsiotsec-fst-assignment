-module(sr_plc).
-export([detect_candy/2, exclude_flavor/2, plc/1]).

detect_candy(PlcPid, Flavor) ->
    PlcPid ! {detect_candy, Flavor}.

exclude_flavor(PlcPid, Flavor) ->
    PlcPid ! {exclude_flavor, Flavor}.

plc(PusherPid) ->
    receive
        {detect_candy, Flavor} ->
            receive
                {exclude_flavor, Flavor} ->
                    sr_pusher:exclude_next(PusherPid)
            after
                100 ->
                    sr_pusher:accept_next(PusherPid)
            end
    end,
    plc(PusherPid).