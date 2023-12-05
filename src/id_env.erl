-module(id_env).
-behaviour(application).
-export([init/0, start_simulation/0, start/2, stop/1]).

start(normal, _) -> spawn_link(fun() -> 
    start_simulation(), 
    receive quit -> exit(normal) end
    end);
start(A, _) -> logger:error("Started simulation with ~p instead of normal", [A]).

stop(ShutDownPid) -> ShutDownPid ! quit.

% Checks if the passed message is \in S (so if it's in U* but not in Y*),
%   i.e. if it's a stimuli coming outside the system. 
is_env_stimuli({load_candy, _}) -> true;
is_env_stimuli({exclude_flavor, _}) -> true;
is_env_stimuli(_) -> false.

% Checks if a message in the real environment is the same as another in the 
%   twin environment. 
matches(A, A) -> true;
matches({_, Ref1, A}, {_, Ref2, A}) -> is_reference(Ref1) andalso is_reference(Ref2);
matches(_, _) -> false.

replicate(MainPid, ProcessPidName, TwinPid) ->
    register(ProcessPidName, self()),
    replicate(MainPid, TwinPid).

% This process will forward all the messages it receives
%   to the Main PID process. Any message which is an 
%   environment stimuli will be replicated to the Twin
%   PID given.
replicate(MainPid, TwinPid) ->
    receive
    A -> MainPid ! A, 
        case is_env_stimuli(A) of
            true -> TwinPid ! {{env_stimuli, erlang:system_time(millisecond)}, A}; % if it's a stimuli, replicate
            false -> ok
        end
    end,
    replicate(MainPid, TwinPid).

% Sends a compiled module to an erlang node.
send_module(Node, Module) ->
    {Module, Bin, File} = code:get_object_code(Module),
    {ResL, BadNodes} = rpc:call(
        Node, code, load_binary, [Module, File, Bin]),
    logger:notice("Sent module ~p to ~p, result was ~p and badnodes ~p", [Module, Node, ResL, BadNodes]),
    ok.

% Sends all needed modules to the mitm and twin nodes.
configure() ->
    net_kernel:connect_node(mitm@localhost),
    send_module(mitm@localhost, sr_mitm),
    net_kernel:connect_node(twin@localhost),
    lists:foreach(fun(E) -> send_module(twin@localhost, E) end, [sr_belt, sr_pusher, sr_plc, sr_rfid_reader, sr_twin]),
    ok.

% A process running this function will expect two 
%   subsequent messages matching between each other. 
% If not, there might be:
% - an intrusion
% - a process in the real world processes a candy 
%   having not finished to process the previous one.
%   In that case, the component will receive two messages 
%   from the twin environment. 
intrusion_detection() ->
    receive
    {main, A} ->
        logger:notice("[CANDY SINK ID] Received ~p", [A]),
        receive
        {twin, B} ->
            case matches(A, B) of
                true -> ok;
                false -> logger:error("[CANDY SINK ID] Received ~p to match over ~p, but no match was possible. This is a possible intrusion.", [B, A])
            end
        end
    end,
    intrusion_detection().

% This function spawns:
% - a main process on this node, wrapped with a process that replicates all environment stimuli;
% - a twin process on the twin@localhost node, wrapped in a process that unpacks the replicated stimuli and forwards them to the digital twin;
% and registers both processes in the respective nodes 
%   to the atom passed as ProcessPidName.
spawn_twin(ProcessPidName, Module, Args, TwinArgs) ->
    MainTag = lists:flatten(io_lib:format("~p MAIN", [Module])),
    TwinTag = lists:flatten(io_lib:format("~p TWIN", [Module])),
    MainPid = spawn_link(Module, init, [MainTag, Args]),
    TwinPid = spawn(twin@localhost, Module, init, [TwinTag, TwinArgs]),
    Chck = spawn(twin@localhost, sr_twin, check_replicate, [TwinPid, ProcessPidName]),
    RepMain = spawn_link(fun() -> replicate(MainPid, ProcessPidName, Chck) end),
    {RepMain, Chck}.


% Sets up the environments.
% 
% Both the main and twin environment have:
% - a pusher, informing the intrusion detection process that a candy has been accepted or rejected;
% - a PLC communicating with the pusher, accepting signals from an HMI in the main environment;
% - a conveyor belt taking candies to the pusher, accepting candies from a HMI in the main environment;
% - a RFID reader reading the candies on the belt and communicating with the PLC.
% 
% Then a HMI for the MITM process is set up on the mitm@localhost node, and it's configured
%   between the RFID reader and the belt in the main@localhost node.
init() ->
    Sink = spawn_link(fun() -> intrusion_detection() end),
    {PusherMain, PusherTwin} = spawn_twin(pusher_pid, sr_pusher, [{sink, Sink}, {env, main}], [{sink, Sink}, {env, twin}]),
    {PLCMain, PLCTwin} = spawn_twin(plc_pid, sr_plc, [{pusher, PusherMain}], [{pusher, PusherTwin}]),
    {BeltMain, BeltTwin} = spawn_twin(belt_pid, sr_belt, [{pusher, PusherMain}], [{pusher, PusherTwin}]),
    MITM = spawn(mitm@localhost, sr_mitm, mitm, []),
    {RFIDReaderMain, _} = spawn_twin(rfid_reader_pid, sr_rfid_reader, [{belt, MITM}, {plc, PLCMain}], [{belt, BeltTwin}, {plc, PLCTwin}]),
    MITM ! {RFIDReaderMain, BeltMain},
    [{mitm, MITM}, {belt, BeltMain}, {plc, PLCMain}].

% Starts a simulation where no lemon candy is provided.
% The scenario is that the candy producer found that all
%   lemon candies are actually bad, therefore they want
%   to reject them all. 
start_simulation() ->
    configure(),
    Env = init(),
    {plc, PLC} = proplists:lookup(plc, Env),
    {belt, Belt} = proplists:lookup(belt, Env),
    {mitm, MITM} = proplists:lookup(mitm, Env),
    timer:sleep(1000),
    spawn_link(fun() -> simulation(Belt, PLC, MITM) end),
    ok.

simulation(Belt, PLC, MITM) ->
    % sr_plc:exclude_flavor(PLC, lemon),
    sr_belt:load_candy(Belt, strawberry),
    timer:sleep(1000),
    sr_belt:load_candy(Belt, orange),
    timer:sleep(1000),
    sr_belt:load_candy(Belt, cherry),
    timer:sleep(1000),
    sr_belt:load_candy(Belt, mou),
    timer:sleep(1000),
    simulation(Belt, PLC, MITM).