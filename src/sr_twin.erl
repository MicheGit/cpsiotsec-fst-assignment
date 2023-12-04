-module(sr_twin).
-export([check_replicate/2]).

% Given a PID of a digital twin, it 
% TODO implementa delta time
check_replicate(TwinPid, ProcessPidName) ->
    register(ProcessPidName, self()),
    check_replicate(TwinPid).

check_replicate(TwinPid) ->
    receive
    {env_stimuli, A} -> 
        logger:debug("Check replicated environment stimuli ~p", [A]),
        TwinPid ! A;
    B -> 
        logger:debug("Check forwarded twin message ~p", [B]),
        TwinPid ! B
    end,
    check_replicate(TwinPid).