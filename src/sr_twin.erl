-module(sr_twin).
-export([check_replicate/1]).

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