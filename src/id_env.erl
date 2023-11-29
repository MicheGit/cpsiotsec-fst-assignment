-module(id_env).
-behaviour(application).
-export([init/0, start_simulation/0, start/2, stop/1, mitm_simulation/0]).

start(normal, _) -> spawn_link(fun() -> 
    start_simulation(), 
    receive quit -> exit(normal) end
    end);
start(A, _) -> logger:error("Started simulation with ~p instead of normal", [A]).


stop(ShutDownPid) -> ShutDownPid ! quit.

is_env_stimuli({load_candy, _}) -> true;
is_env_stimuli({exclude_flavor, _}) -> true;
is_env_stimuli(_) -> false.

matches(A, A) -> true;
matches({_, Ref1, A}, {_, Ref2, A}) -> is_reference(Ref1) andalso is_reference(Ref2);
matches(_, _) -> false.

replicate(Main, Twin) ->
    receive
    A -> Main ! A, 
        case is_env_stimuli(A) of
            true -> Twin ! {env_stimuli, A}; % if it's a stimuli, replicate
            false -> ok
        end
    end,
    replicate(Main, Twin).

% TODO implementa delta time
check_replicate(Twin) ->
    receive
    {env_stimuli, A} -> 
        logger:debug("Check replicated environment stimuli ~p", [A]),
        Twin ! A;
    B -> 
        logger:debug("Check forwarded twin message ~p", [B]),
        Twin ! B
    end,
    check_replicate(Twin).

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


intrusion_detection() ->
    receive
    A ->
        logger:notice("[CANDY SINK ID] Received ~p", [A]),
        receive
        B ->
            case matches(A, B) of
                true -> ok;
                false -> logger:error("[CANDY SINK ID] Received ~p to match over ~p, but no match was possible. This is a possible intrusion.", [B, A])
            end
        end
    end,
    intrusion_detection().

spawn_twin(Module, Function, Args, TwinArgs) ->
    MainTag = lists:flatten(io_lib:format("~p MAIN", [Module])),
    TwinTag = lists:flatten(io_lib:format("~p TWIN", [Module])),
    Main = spawn_link(Module, Function, [MainTag, Args]),
    Twin = spawn_link(Module, Function, [TwinTag, TwinArgs]),
    Chck = spawn_link(fun() -> check_replicate(Twin) end),
    RepMain = spawn_link(fun() -> replicate(Main, Chck) end),
    {RepMain, Chck}.

init() ->
    Sink = spawn_link(fun() -> intrusion_detection() end),
    {PusherMain, PusherTwin} = spawn_twin(sr_pusher, init, [{sink, Sink}], [{sink, Sink}]),
    {PLCMain, PLCTwin} = spawn_twin(sr_plc, init, [{pusher, PusherMain}], [{pusher, PusherTwin}]),
    {BeltMain, BeltTwin} = spawn_twin(sr_belt, init, [{pusher, PusherMain}], [{pusher, PusherTwin}]),
    MITM = spawn_link(sr_mitm, mitm, []),
    {RFIDReaderMain, _} = spawn_twin(sr_rfid_reader, init, [{belt, MITM}, {plc, PLCMain}], [{belt, BeltTwin}, {plc, PLCTwin}]),
    MITM ! {RFIDReaderMain, BeltMain},
    [{mitm, MITM}, {belt, BeltMain}, {plc, PLCMain}].

% Starts a simulation where no lemon candy is provided.
% The scenario is that the candy producer found that all
%   lemon candies are actually bad, therefore they want
%   to reject them all. 
start_simulation() ->
    Env = init(),
    {plc, PLC} = proplists:lookup(plc, Env),
    {belt, Belt} = proplists:lookup(belt, Env),
    {mitm, MITM} = proplists:lookup(mitm, Env),
    spawn_link(fun() -> simulation(Belt, PLC, MITM) end),
    {PLC, Belt, MITM}.

simulation(Belt, PLC, MITM) ->
    sr_plc:exclude_flavor(PLC, lemon),
    timer:sleep(1000),
    sr_belt:load_candy(Belt, strawberry),
    timer:sleep(1000),
    sr_belt:load_candy(Belt, orange),
    timer:sleep(1000),
    sr_belt:load_candy(Belt, cherry),
    timer:sleep(1000),
    sr_belt:load_candy(Belt, mou),
    timer:sleep(1000),
    simulation(Belt, PLC, MITM).

% Starts a simulation and returns the reference PID to 
% the man in the middle. 
% Injecting packet with `MITM ! {inject_packet, {candy, lemon}}` 
% will cause the real system to reject a non-lemon candy.
% The virtual environment will detect the intrusion.
% Eg.
%   $ > sh start.sh
%   Eshell V14.1 (press Ctrl+G to abort, type help(). for help)
%   1> MITM = id_env:mitm_simulation().
% now the system will start to log things
% wait a little and then
%   2> MITM ! {inject_packet, {candy, lemon}}.
mitm_simulation() ->
    {_, _, MITM} = start_simulation(),
    MITM.