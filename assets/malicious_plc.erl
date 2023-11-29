-module(sr_plc).
-export([detect_candy/2, exclude_flavor/2, init/2]).

detect_candy(PlcPid, Flavor) ->
    PlcPid ! {detect_candy, Flavor}.

exclude_flavor(PlcPid, Flavor) ->
    PlcPid ! {exclude_flavor, Flavor}.

init(LogName, Args) ->
    {pusher, PusherPid} = proplists:lookup(pusher, Args),
    plc(LogName, PusherPid, none).

plc(LogName, PusherPid, _) ->
    logger:info("[~p] User decided to exclude all candies. Awaiting for detection", [LogName]),
    receive {detect_candy, Flavor} -> 
        receive {exclude_flavor, _} -> ok end,
        logger:info("[~p] User decided to exclude all candies and detected flavor is ~p", [LogName, Flavor]),
        sr_pusher:exclude_next(PusherPid),
        plc(LogName, PusherPid, all)
    end.