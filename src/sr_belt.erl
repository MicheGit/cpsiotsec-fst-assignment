-module(sr_belt).
-export([read_flavor/1, load_candy/2, init/2]).

read_flavor(BeltPid) ->
    Ref = make_ref(),
    BeltPid ! {self(), Ref, read_flavor},
    receive
        {Ref, nothing} -> nothing;
        {Ref, {candy, Flavor}} -> Flavor
    end.

% load_candy is a stimuli in S (in U* but not in Y*)
load_candy(BeltPid, Flavor) ->
    BeltPid ! {load_candy, {candy, Flavor}}.

init(LogName, Args) ->
    {pusher, PusherPid} = proplists:lookup(pusher, Args),
    belt(LogName, PusherPid, nothing).

% The conveyor belt.
% This process models a belt that has:
% - max one candy on it;
% - can inform a RFID reader about the flavour of the candy on the belt;
% - takes the candy to a pusher that decides its fate;
% - accepts a new candy after dropping the last one.
belt(LogName, PusherPid, Candy) ->
    logger:info("[~p] Start with Candy = ~p", [LogName, Candy]),
    logger:info("[~p] Awaiting for read", [LogName]),
    receive
        {SenderPid, Ref, read_flavor} -> SenderPid ! {Ref, Candy}
    after 
        500 -> continue
    end,
    case Candy of
        {candy, _} -> 
            logger:info("[~p] Hello reader! There is ~p on the belt", [LogName, Candy]),
            sr_pusher:push_candy(PusherPid, Candy);
        nothing -> logger:info("[~p] Hello reader! There is no candy on me", [LogName])
    end,
    receive
        {load_candy, C} -> 
            logger:info("[~p] Got new Candy = ~p", [LogName, C]),
            belt(LogName, PusherPid, C)
    after
        100 -> 
            logger:info("[~p] No candy on the belt", [LogName]),
            belt(LogName, PusherPid, nothing)
    end.

