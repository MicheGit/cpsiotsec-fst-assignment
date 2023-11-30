-module(sr_plc).
-export([detect_candy/2, exclude_flavor/2, init/2, plc/3]).

detect_candy(PlcPid, Flavor) ->
    PlcPid ! {detect_candy, Flavor}.

exclude_flavor(PlcPid, Flavor) ->
    PlcPid ! {exclude_flavor, Flavor}.

init(LogName, Args) ->
    register(plc_pid, self()),
    {pusher, PusherPid} = proplists:lookup(pusher, Args),
    plc(LogName, PusherPid, none).

plc(LogName, PusherPid, _) ->
    logger:info("[~p] User decided to exclude all candies. Awaiting for detection", [LogName]),
    receive {detect_candy, Flavor} -> 
        get_last_exclude_flavor(all),
        logger:info("[~p] User decided to exclude all candies and detected flavor is ~p", [LogName, Flavor]),
        sr_pusher:exclude_next(PusherPid),
        plc(LogName, PusherPid, all)
    end.

get_last_exclude_flavor(Last) ->
    receive
        {exclude_flavor, Next} -> get_last_exclude_flavor(Next)
    after
        100 -> Last
    end.