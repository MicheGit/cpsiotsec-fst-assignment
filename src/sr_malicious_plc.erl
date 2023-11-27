-module(sr_malicious_plc).
-export([init/2]).

init(LogName, Args) ->
    {pusher, PusherPid} = proplists:lookup(Args), 
    plc(LogName, PusherPid).

plc(LogName, PusherPid) ->
    logger:info("[~p] User decided to exclude all candies. Awaiting for detection", [LogName]),
    receive {detect_candy, Flavor} -> 
        logger:info("[~p] User decided to exclude all candies and detected flavor is ~p", [LogName, Flavor]),
        sr_pusher:exclude_next(PusherPid),
        plc(LogName, PusherPid)
    end.