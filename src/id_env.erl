-module(id_env).
-behaviour(application).
-export([init/0, start_simulation/0, start/2, stop/1]).

start(normal, _) -> start_simulation();
start(A, _) -> logger:error("Started simulation with ~p instead of normal", [A]).


stop(StartReturn) -> StartReturn ! quit.

is_env_stimuli({load_candy, _}) -> true;
is_env_stimuli({exclude_flavor, _}) -> true;
is_env_stimuli(_) -> false.

matches(A, A) -> true;
matches({_, Ref1, A}, {_, Ref2, A}) -> is_reference(Ref1) andalso is_reference(Ref2);
matches(_, _) -> false.

replicate(Pid, Sink) ->
    receive
        A -> Pid ! A, Sink ! {replicated_message, A}
    end,
    replicate(Pid, Sink).

check_replicate_help(Pid, Replicated, Expected) ->
    {Matched, NewReplicated, NewExpected} = receive
    {replicated_message, A} -> case is_env_stimuli(A) of
        true -> {{ok, A}, Replicated, Expected};
        false -> match_queues([A|Replicated], Expected)
    end;
    B -> match_queues(Replicated, [B|Expected])
    end,
    case Matched of
        {ok, Value} -> Pid ! Value;
        none -> ok
    end,
    check_replicate_help(Pid, NewReplicated, NewExpected).

list_find(_, []) -> {nothing, []};
list_find(Pred, [X|XS]) -> case Pred(X) of
    true -> {X, XS};
    false -> 
        {Y, YS} = list_find(Pred, XS),
        {Y, [X|YS]}
    end.

match_queues(Repl, []) -> {none, Repl, []};
match_queues([], Exp) -> {none, [], Exp};
match_queues([R|RS], Exp) -> 
    Result = list_find(fun(E) -> matches(E, R) end, Exp),
    case Result of
    {Something, ES} -> {Something, RS, ES};
    {nothing, Exp} -> 
        {Smth, RS1, ES} = match_queues(RS, Exp),
        {Smth, [R|RS1], ES}
    end.


check_replicate(Pid) -> check_replicate_help(Pid, [], []).
                

spawn_twin(Module, Function, Args, TwinArgs) ->
    MainTag = lists:flatten(io_lib:format("~p MAIN", [Module])),
    TwinTag = lists:flatten(io_lib:format("~p TWIN", [Module])),
    Main = spawn_link(Module, Function, [MainTag, Args]),
    Twin = spawn_link(Module, Function, [TwinTag, TwinArgs]),
    Chck = spawn_link(fun() -> check_replicate(Twin) end),
    RepMain = spawn_link(fun() -> replicate(Main, Chck) end),
    {RepMain, Twin}.

init() ->
    Sink = spawn_link(sr_sink, sink, []),
    {PusherMain, PusherTwin} = spawn_twin(sr_pusher, init, [{sink, Sink}], [{sink, Sink}]),
    {PLCMain, PLCTwin} = spawn_twin(sr_plc, init, [{pusher, PusherMain}], [{pusher, PusherTwin}]),
    {BeltMain, BeltTwin} = spawn_twin(sr_belt, init, [{pusher, PusherMain}], [{pusher, PusherTwin}]),
    MITM = spawn_link(sr_mitm, mitm, []),
    {RFIDReaderMain, _} = spawn_twin(sr_rfid_reader, init, [{belt, MITM}, {plc, PLCMain}], [{belt, BeltTwin}, {plc, PLCTwin}]),
    MITM ! {RFIDReaderMain, BeltMain},
    [{mitm, MITM}, {belt, BeltMain}, {plc, PLCMain}].

start_simulation() ->
    Env = init(),
    timer:sleep(500),
    {plc, PLC} = proplists:lookup(plc, Env),
    sr_plc:exclude_flavor(PLC, strawberry),
    {belt, Belt} = proplists:lookup(belt, Env),
    sr_belt:load_candy(Belt, lemon),
    % {mitm, MITM} = proplists:lookup(mitm, Env),
    % MITM ! {inject_packet, {candy, strawberry}},
    receive quit -> exit(normal) end.

