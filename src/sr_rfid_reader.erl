-module(sr_rfid_reader).
-export([rfid_reader/2]).

rfid_reader(BeltPid, PlcPid) ->
    Candy = sr_belt:read_candy(BeltPid),
    case Candy of
        {candy, Flavor} -> sr_plc:detect_candy(PlcPid, Flavor);
        _ -> ok
    end,
    rfid_reader(BeltPid, PlcPid).