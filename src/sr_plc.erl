-module(sr_plc).
-export([detect_candy/2, exclude_flavor/2, plc/1]).

detect_candy(PlcPid, Flavor) ->
    PlcPid ! {detect_candy, Flavor}.

exclude_flavor(PlcPid, Flavor) ->
    PlcPid ! {exclude_flavor, Flavor}.

plc(PusherPid) ->
    plc(PusherPid, none).
plc(PusherPid, ExcludingFlavor) ->
    logger:info("[PLC PROC] Starting plc process, awaiting for user exclusion"),
    NEF = get_last_exclude_flavor(ExcludingFlavor),
    logger:info("[PLC PROC] User decided to exclude ~p. Awaiting for detection", [NEF]),
    receive
        {detect_candy, NEF} -> sr_pusher:exclude_next(PusherPid),
            plc(PusherPid);
        {detect_candy, _} -> sr_pusher:accept_next(PusherPid),
            plc(PusherPid, NEF)
    end.

get_last_exclude_flavor(Last) ->
    receive
        {exclude_flavor, Next} -> get_last_exclude_flavor(Next)
    after
        100 -> Last
    end.
