-module(sr_mitm).
-export([mitm/3]).

mitm(Behavior, RFIDReaderPid, BeltPid) ->
    receive
        {RFIDReaderPid, Ref, _} = A -> 
            BeltPid ! A,
            receive
                {Ref, _} = B ->
                    case Behavior of
                        sneaky -> RFIDReaderPid ! B;
                        bad ->
                            receive
                                {inject_packet, Substitute} -> 
                                    RFIDReaderPid ! {Ref, Substitute}
                            end
                    end
            end
    end,
    NewBehavior = receive
        {new_behavior, Val} -> Val
    after
        100 -> Behavior
    end,
    mitm(NewBehavior, RFIDReaderPid, BeltPid).
