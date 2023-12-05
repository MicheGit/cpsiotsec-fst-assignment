-module(sr_twin).
-export([check_replicate/2]).

% Given a PID of a digital twin, it
%   routes all twin messages to it and
%   replicates the environment stimuli 
%   when they are consistent between
%   each other.
check_replicate(TwinPid, ProcessPidName) ->
    register(ProcessPidName, self()),
    stimuli_replication(TwinPid, nothing).

stimuli_replication(TwinPid, State) ->
    {MSG, NewState} = receive
    {{env_stimuli, ReplicationTimestamp}, A} -> 
        logger:debug("Check replicated environment stimuli ~p at timestamp ~p", [A, ReplicationTimestamp]),
        {FirstStimuliTimestamp, T0, PreviousStimuliTimestamp} = case State of
            nothing -> % first time
                {ReplicationTimestamp, erlang:system_time(millisecond), ReplicationTimestamp};
            State -> State
        end,
        DeltaPrevious = ReplicationTimestamp - PreviousStimuliTimestamp,
        if 
            DeltaPrevious < 0 -> logger:error("Received a stimuli with an earlier timestamp. It is unclear if this is due to an intrusion or a real fault in the system. Bailing out."),
                exit(inconsistent_stimuli);
            true -> continue
        end,
        DeltaFirst = ReplicationTimestamp - FirstStimuliTimestamp,
        TSleep = (T0 + DeltaFirst) - erlang:system_time(millisecond),
        if
            TSleep > 0 -> timer:sleep(TSleep);
            true -> ok % State replication delay! Replicating ASAP is good.
        end,
        {A, {FirstStimuliTimestamp, T0, ReplicationTimestamp}};
    B -> 
        logger:debug("Check forwarded twin message ~p", [B]),
        {B, State}
    end,
    TwinPid ! MSG,
    stimuli_replication(TwinPid, NewState).