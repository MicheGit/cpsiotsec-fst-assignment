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
    {replicated_message, A} -> match_queues([A|Replicated], Expected);
    B -> match_queues(Replicated, [B|Expected])
    end,
    case Matched of
        {ok, Value} -> Pid ! Value;
        none -> ok
    end,
    check_replicate_help(Pid, NewReplicated, NewExpected).

list_find(Pred, []) -> {nothing, []};
list_find(Pred, [X|XS]) -> case Pred(X) of
    true -> {X, XS};
    false -> 
        {Y, YS} = list_find(Pred, XS),
        {Y, [X|YS]}
    end.

match_queues(Repl, []) -> {none, Repl, []};
match_queues([], Exp) -> {none, [], Exp};
match_queues([R|RS], Exp) -> 
    Result = list_find(fun(E) -> matches(E, R) end, Exp);
    case Result of
    {Something, ES} -> {Something, RS, ES};
    {nothing, Exp} -> 
        {Smth, RS1, ES} = match_queues(RS, Exp),
        {Smth, [R|RS1], ES}
    end.


check_replicate(Pid) ->
    receive 
    {replicated_message, A} ->
        logger:debug("Spotted replicated_message ~p", [A]),
        case is_env_stimuli(A) of
            true -> 
                logger:debug("~p is an environment stimuli", [A]),
                Pid ! A; % should be replicated here, however in these cases replication = id
            false -> 
                logger:debug("~p is NOT an environment stimuli", [A]),
                {ok, B, Discarded} = until_matches(A, []), %% otherwise, intrusion!
                Pid ! B,
                logger:debug("Discarded is ~p", Discarded),
                lists:foreach(fun(M) -> logger:debug("Putting back ~p into queue", [M]), self() ! M end, Discarded) % puts back into queue discarded messages
        end
    end,
    check_replicate(Pid).

until_matches(A, Discarded) ->
    receive
    B -> 
        logger:debug("Want to match ~p with ~p", [A, B]),
        case matches(A, B) of
            true -> logger:debug("Matched ~p with ~p", [A, B]), {ok, B, Discarded};
            false -> logger:debug("Not matched ~p with ~p", [A, B]), until_matches(A, [B|Discarded])
        end
    after
        1000 -> error(lists:flatten(io_lib:format("Intrusion detected! Message ~p was replicated but did't match with any in U^*", [A]))) % if after X seconds the message doesn't arrive, there is an intrusion
    end.
                

spawn_twin(Module, Function, Args, TwinArgs) ->
    Main = spawn_link(Module, Function, Args),
    Twin = spawn_link(Module, Function, TwinArgs),
    Chck = spawn_link(fun() -> check_replicate(Twin) end),
    RepMain = spawn_link(fun() -> replicate(Main, Chck) end),
    {RepMain, Twin}.

init() ->
    Sink = spawn_link(sr_sink, sink, []),
    {PusherMain, PusherTwin} = spawn_twin(sr_pusher, pusher, [Sink], [Sink]),
    {PLCMain, PLCTwin} = spawn_twin(sr_plc, plc, [PusherMain], [PusherTwin]),
    {BeltMain, BeltTwin} = spawn_twin(sr_belt, belt, [PusherMain], [PusherTwin]),
    MITM = spawn_link(sr_mitm, mitm, []),
    {RFIDReaderMain, _} = spawn_twin(sr_rfid_reader, rfid_reader, [MITM, PLCMain], [BeltTwin, PLCTwin]),
    MITM ! {RFIDReaderMain, BeltMain},
    [{mitm, MITM}, {belt, BeltMain}, {plc, PLCMain}].

start_simulation() ->
    Env = init(),
    timer:sleep(500),
    {belt, Belt} = proplists:lookup(belt, Env),
    sr_belt:load_candy(Belt, lemon),
    receive quit -> exit(normal) end.

