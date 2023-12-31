-module(sr_mitm).
-export([mitm/0]).

% The Man-In-The-Middle process starts
%   awaitng for the PIDs of the belt and the
%   reader. It then will listen for any incoming
%   communication, directed to the belt and
%   forward it to the actual recipient.
% Unless, a user can provide this process
%   a message like {inject_packet, {candy, Flavor}}
%   to replace the next read candy on the belt: 
%   this will not replace a "nothing" read.
mitm() ->
    register(mitm_pid, self()),
    % For the first time, I need to know where to find the reader and the belt
    receive {RFIDReaderPid, BeltPid} ->
        mitm(RFIDReaderPid, BeltPid)
    end.

mitm(RFIDReaderPid, BeltPid) ->
    logger:debug("[MITM PROC] Awaiting for a message from the reader to the belt."),
    receive {_, Ref, Body} -> 
        logger:debug("[MITM PROC] I've intercepted the read request, forwarding. Now I listen to response"),
        BeltPid ! {self(), Ref, Body},
        receive 
        {Ref, nothing} = B -> RFIDReaderPid ! B;
        {Ref, _} = B ->
            logger:debug("[MITM PROC] Now I'll wait a packet to inject"), 
            receive
                {inject_packet, Substitute} -> logger:debug("[MITM PROC] Got a packet ~p to inject over ~p!", [Substitute, B]), 
                    RFIDReaderPid ! {Ref, Substitute}
            after
                100 -> logger:debug("[MITM PROC] Stop waiting, they'll find out"), 
                    RFIDReaderPid ! B
            end
        end
    end,
    mitm(RFIDReaderPid, BeltPid).
