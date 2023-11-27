-module(sr_rfid_reader).
-export([init/2]).

init(LogName, Args) ->
    {belt, BeltPid} = proplists:lookup(belt, Args),
    {plc, PlcPid} = proplists:lookup(plc, Args),
    rfid_reader(LogName, BeltPid, PlcPid).

rfid_reader(LogName, BeltPid, PlcPid) ->
    logger:info("[~p] Starting up, reading Candy", [LogName]),
    Flavor = sr_belt:read_flavor(BeltPid),
    logger:info("[~p] Candy on belt has ~p flavour", [LogName, Flavor]),
    if 
        Flavor == nothing -> continue;
        true -> logger:info("[~p] Sending detection to the plc", [LogName]),
            sr_plc:detect_candy(PlcPid, Flavor)
    end,
    timer:sleep(100),
    rfid_reader(LogName, BeltPid, PlcPid).