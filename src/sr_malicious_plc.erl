-module(sr_malicious_plc).
-export([plc/1]).

plc(PusherPid) ->
    logger:info("[PLC PROC] User decided to exclude all candies. Awaiting for detection"),
    receive {detect_candy, Flavor} -> 
        logger:info("[PLC PROC] User decided to exclude all candies and detected flavor is ~p", [Flavor]),
        sr_pusher:exclude_next(PusherPid),
        plc(PusherPid)
    end.