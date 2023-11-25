-module(sr_env).
-export([start_simulation/0]).

replicate(Pid, Sink) ->
    receive
        A -> Pid ! A, Sink ! A
    end,
    replicate(Pid, Sink).

init() ->
    Sink = spawn(sr_sink, sink, []),
    Pusher = spawn(sr_pusher, pusher, [Sink]),
    RepPusher = spawn(fun() -> replicate(Pusher, Sink) end),
    PLC = spawn(sr_malicious_plc, plc, [RepPusher]),
    RepPLC = spawn(fun() -> replicate(PLC, Sink) end),
    Belt = spawn(sr_belt, belt, [RepPusher]),
    MITM = spawn(sr_mitm, mitm, []),
    RepBelt = spawn(fun() -> replicate(Belt, Sink) end),
    RFIDReader = spawn(sr_rfid_reader, rfid_reader, [MITM, RepPLC]),
    RepRFIDReader = spawn(fun() -> replicate(RFIDReader, Sink) end),
    MITM ! {RepRFIDReader, RepBelt},
    [{mitm, MITM}, {belt, RepBelt}, {plc, RepPLC}].

start_simulation() ->
    Env = init(),
    {mitm, MITM} = proplists:lookup(mitm, Env),
    {belt, Belt} = proplists:lookup(belt, Env),
    {plc, PLC} = proplists:lookup(plc, Env),
    spawn(fun() -> simulation(Belt, PLC, MITM) end).

simulation(Belt, PLC, MITM) ->
    sr_belt:load_candy(Belt, lemon),
    timer:sleep(1000),
    sr_belt:load_candy(Belt, strawberry),
    sr_plc:exclude_flavor(PLC, lemon),
    MITM ! {inject_packet, {candy, lemon}},
    timer:sleep(1000),
    sr_belt:load_candy(Belt, orange),
    timer:sleep(1000),
    sr_belt:load_candy(Belt, cherry),
    timer:sleep(1000),
    sr_belt:load_candy(Belt, mou),
    timer:sleep(1000),
    simulation(Belt, PLC, MITM). 
    
