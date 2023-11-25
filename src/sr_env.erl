-module(sr_env).
-export([start_simulation/0]).

init() ->
    Sink = spawn(sr_sink, sink, []),
    Pusher = spawn(sr_pusher, pusher, [Sink]),
    PLC = spawn(sr_malicious_plc, plc, [Pusher]),
    Belt = spawn(sr_belt, belt, [Pusher, nothing]),
    MITM = spawn(sr_mitm, mitm, [Belt]),
    spawn(sr_rfid_reader, rfid_reader, [MITM, PLC]),
    [{mitm, MITM}, {belt, Belt}, {plc, PLC}].

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
    
