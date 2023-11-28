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

replicate(Main, Twin, IntrusionDetection) ->
    receive
    A -> Main ! A, 
        case is_env_stimuli(A) of
            true -> Twin ! {env_stimuli, A}; % if it's a stimuli, replicate
            false -> IntrusionDetection ! {main, A} % otherwise check for intrusion
        end
    end,
    replicate(Main, Twin, IntrusionDetection).

check_replicate(Twin, IntrusionDetection) ->
    receive
    {env_stimuli, A} -> 
        logger:notice("Check replicated environment stimuli ~p", [A]),
        Twin ! A;
    B -> 
        logger:notice("Check forwarded twin message ~p", [B]),
        IntrusionDetection ! {twin, B}
    end,
    check_replicate(Twin, IntrusionDetection).

list_find(_, []) -> {nothing, []};
list_find(Pred, [X|XS]) -> case Pred(X) of
    true -> {X, XS};
    false -> 
        {Y, YS} = list_find(Pred, XS),
        {Y, [X|YS]}
    end.


intrusion_detection(LogName, AwaitingForCheck) ->
    NewState = receive
    {main, A} -> 
        logger:notice("[ID ~p] Got message {main, ~p}", [LogName, A]),
        Checker = spawn(fun() -> require_replica(LogName, A) end),
        [Checker|AwaitingForCheck];
    {twin, B} ->
        logger:notice("[ID ~p] Got message {twin, ~p}", [LogName, B]),
        lists:foreach(fun(Checker) -> Checker ! B end, AwaitingForCheck),
        AwaitingForCheck
    end,
    intrusion_detection(LogName, NewState).

require_replica(LogName, A) ->
    Awaiter = spawn(fun() -> 
        receive matched -> ok 
        after 5000 -> logger:error("Intrusion detected! No match within 5 seconds for message ~p", [A])
        end
    end),
    require_replica(LogName, A, Awaiter).

require_replica(LogName, A, Awaiter) ->
    receive
    B -> case matches(A, B) of
        true ->
            logger:notice("[ID ~p] Succesfully ~p over ~p", [LogName, A, B]),
            Awaiter ! matched;
        false -> 
            logger:notice("[ID ~p] Couldn't match ~p over ~p", [LogName, A, B]),
            require_replica(A, Awaiter)
        end
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
                

spawn_twin(Module, Function, Args, TwinArgs) ->
    IntrusionDetection = spawn_link(fun() -> intrusion_detection(Module, []) end),
    MainTag = lists:flatten(io_lib:format("~p MAIN", [Module])),
    TwinTag = lists:flatten(io_lib:format("~p TWIN", [Module])),
    Main = spawn_link(Module, Function, [MainTag, Args]),
    Twin = spawn_link(Module, Function, [TwinTag, TwinArgs]),
    Chck = spawn_link(fun() -> check_replicate(Twin, IntrusionDetection) end),
    RepMain = spawn_link(fun() -> replicate(Main, Chck, IntrusionDetection) end),
    {RepMain, Chck}.

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

