-module(sr_plc).
-export([detect_candy/2, exclude_flavor/2, init/2, plc/3]).


% Given the PID of a PLC and a flavor,
%   informs the PLC that a candy of 
%   such flavor has been seen on the
%   conveyor belt.
detect_candy(PlcPid, Flavor) ->
    PlcPid ! {detect_candy, Flavor}.


% Given the PID of a PLC and a flavor,
%   informs the PLC that the next candy
%   with such flavor must be discarded.
exclude_flavor(PlcPid, Flavor) ->
    PlcPid ! {exclude_flavor, Flavor}.

init(LogName, Args) ->
    {pusher, PusherPid} = proplists:lookup(pusher, Args),
    plc(LogName, PusherPid, none).


% Simulates a PLC in an industrial system.
% When it receives the detection of a candy, 
%   unfolds the message queue to get the last
%   user selection. If the flavors match, 
%   then it informs the pusher to reject the 
%   next candy it receives.
plc(LogName, PusherPid, ExcludingFlavor) ->
    logger:info("[~p] User decided to exclude ~p. Awaiting for detection", [LogName, ExcludingFlavor]),
    receive {detect_candy, Flavor} -> 
        logger:info("[~p] Starting plc process, awaiting for user exclusion", [LogName]),
        NEF = get_last_exclude_flavor(ExcludingFlavor),
        logger:info("[~p] User decided to exclude ~p and detected flavor is ~p", [LogName, NEF, Flavor]),
        case Flavor of
            NEF -> sr_pusher:exclude_next(PusherPid),
                sr_plc:plc(LogName, PusherPid, none);
            _ -> sr_pusher:accept_next(PusherPid),
                sr_plc:plc(LogName, PusherPid, NEF)
        end
    end.

get_last_exclude_flavor(Last) ->
    receive
        {exclude_flavor, Next} -> get_last_exclude_flavor(Next)
    after
        100 -> Last
    end.
