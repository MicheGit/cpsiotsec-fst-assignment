-module(sr_rfid_reader).
-export([rfid_reader/2]).

rfid_reader(BeltPid, PlcPid) ->
    logger:info("[RFID READER PROC] Starting up, reading Candy"),
    Flavor = sr_belt:read_flavor(BeltPid),
    logger:info("[RFID READER PROC] Candy on belt has ~p flavour", [Flavor]),
    if 
        Flavor == nothing -> continue;
        true -> logger:info("[RFID READER PROC] Sending detection to the plc"),
            sr_plc:detect_candy(PlcPid, Flavor)
    end,
    timer:sleep(100),
    rfid_reader(BeltPid, PlcPid).