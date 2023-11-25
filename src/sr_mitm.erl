-module(sr_mitm).
-export([mitm/2]).

mitm(Behavior, BeltPid) ->
    logger:debug("[MITM PROC] I'm behaving ~p. Awaiting for a message from the reader to the belt.", [Behavior]),
    receive {RFIDReaderPid, Ref, Body}-> 
        logger:debug("[MITM PROC] I've intercepted the read request, forwarding. Now I listen to response"),
        BeltPid ! {self(), Ref, Body},
        receive {Ref, _} = B ->
            case Behavior of
                silent ->
                    logger:debug("[MITM PROC] I'll be a good boy for now..."), 
                    RFIDReaderPid ! B;
                malicious ->
                    logger:debug("[MITM PROC] Now I'll wait a packet to inject"), 
                    receive
                        {inject_packet, Substitute} -> 
                            RFIDReaderPid ! {Ref, Substitute}
                    end
            end
        end
    end,
    logger:debug("[MITM PROC] Notify me how I should behave now."), 
    NewBehavior = receive
        {new_behavior, Val} -> Val
    after
        100 -> Behavior
    end,
    mitm(NewBehavior, BeltPid).
