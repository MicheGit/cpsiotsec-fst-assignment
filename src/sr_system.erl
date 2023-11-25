-module(sr_system).

is_env_stimuli({load_candy, _}) -> true;
is_env_stimuli({exclude_flavor, _}) -> true;
is_env_stimuli(_) -> false.

matches(A, A) -> true;
matches({_, Ref1, A}, {_, Ref2, A}) -> is_reference(Ref1) andalso is_reference(Ref2);
matches(_, _) -> false.

% S is the set of stimuli, defined as all the inputs of the real
%   system that are not outputs of any component in the real system,
%   i.e. all the inputs that are environment inputs.
% If a replicated_message is in S then we don't expect its counterpart
%   from the twin environment.
check_consistency(Twin) ->
    receive
        {replicated_message, ReplMessage} -> case is_env_stimuli(ReplMessage) of
            true -> Twin ! ReplMessage; % if it's an environment stimuli (eg. human input) we just replicate it and drop it from the queue
            false -> expect_twin(ReplMessage, Twin, 100)
        end
    end,
    check_consistency(Twin).

expect_twin(ReplMessage, _, 0) ->
    logger:error(lists:flatten(io_lib:format("A matching message of ~p within 1.5s was not found.", [ReplMessage]))),
    error("No match of a replicated message");
expect_twin(ReplMessage, Twin, Try) ->
    receive
        {twin_message, TwinMessage} -> case matches(ReplMessage, TwinMessage) of
            true -> Twin ! TwinMessage; % if the two matches, send the twin message to the receiver
            false -> expect_twin(ReplMessage, Twin, Try - 1)% if the two dont matches we have to search for a matching twin message in the queue
        end
    after 1500 -> 
        expect_twin(ReplMessage, Twin, 0)
    end.
    



replicate(Main, Twin) ->
    receive
        A -> Main ! A, Twin ! {twin_message, A}
    end,
    replicate(Main, Twin).
% sr_pusher:push_candy(Pusher, {candy, lemon})