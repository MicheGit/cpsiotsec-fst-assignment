-module(sr_belt).
-export([read_flavor/1, load_candy/2, belt/1]).

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

belt(PusherPid) -> belt(PusherPid, nothing).

% The conveyor belt.
% This process models a belt that has:
% - max one candy on it;
% - can inform a RFID reader about the flavour of the candy on the belt;
% - takes the candy to a pusher that decides its fate;
% - accepts a new candy after dropping the last one.
belt(PusherPid, Candy) ->
    logger:info("[BELT PROC] Start with Candy = ~p", [Candy]),
    logger:info("[BELT PROC] Awaiting for read"),
    receive
        {SenderPid, Ref, read_flavor} -> SenderPid ! {Ref, Candy}
    after 
        500 -> continue
    end,
    case Candy of
        {candy, _} -> 
            logger:info("[BELT PROC] Hello reader! There is ~p on the belt", [Candy]),
            sr_pusher:push_candy(PusherPid, Candy);
        nothing -> logger:info("[BELT PROC] Hello reader! There is no candy on me")
    end,
    receive
        {load_candy, C} -> 
            logger:info("[BELT PROC] Got new Candy = ~p", [C]),
            belt(PusherPid, C)
    after
        100 -> 
            logger:info("[BELT PROC] No candy on the belt"),
            belt(PusherPid, nothing)
    end.

