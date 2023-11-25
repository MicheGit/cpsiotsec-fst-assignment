-module(id_env).
-export([init/0]).

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

check_replicate(Pid) ->
    receive 
    {replicated_message, A} ->
        logger:debug("Spotted replicated_message ~p", [A]),
        {ok, B, Discarded} = until_matches(A, []), %% otherwise, intrusion!
        Pid ! B,
        lists:foreach(fun(M) -> self() ! M end, Discarded) % puts back into queue discarded messages
    end,
    check_replicate(Pid).

until_matches(A, Discarded) ->
    receive
    B -> 
        logger:debug("Want to match ~p with ~p", [A, B]),
        case matches(A, B) of
            true -> {ok, B, Discarded};
            false -> until_matches(A, [B|Discarded])
        end
    after
        2000 -> error("Intrusion detected!") % if after X seconds the message doesn't arrive, there is an intrusion
    end.
                

spawn_twin(Module, Function, Args, TwinArgs) ->
    Main = spawn(Module, Function, Args),
    Twin = spawn(Module, Function, TwinArgs),
    Chck = spawn_link(fun() -> check_replicate(Twin) end),
    RepMain = spawn_link(fun() -> replicate(Main, Chck) end),
    {RepMain, Twin}.

init() ->
    Sink = spawn(sr_sink, sink, []),
    {PusherMain, PusherTwin} = spawn_twin(sr_pusher, pusher, [Sink], [Sink]),
    {PLCMain, PLCTwin} = spawn_twin(sr_plc, plc, [PusherMain], [PusherTwin]),
    {BeltMain, BeltTwin} = spawn_twin(sr_belt, belt, [PusherMain], [PusherTwin]),
    MITM = spawn(sr_mitm, mitm, []),
    {RFIDReaderMain, _} = spawn_twin(sr_rfid_reader, rfid_reader, [MITM, PLCMain], [BeltTwin, PLCTwin]),
    MITM ! {RFIDReaderMain, BeltMain},
    [{mitm, MITM}, {belt, BeltMain}, {plc, PLCMain}].