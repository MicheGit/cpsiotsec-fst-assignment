-module(sr_mitm).
-export([mitm/1]).

mitm(BeltPid) ->
    logger:debug("[MITM PROC] Awaiting for a message from the reader to the belt."),
    receive {RFIDReaderPid, Ref, Body} -> 
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
    mitm(BeltPid).
