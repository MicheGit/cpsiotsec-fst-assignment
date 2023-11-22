-module(sr_belt).
-export([read_candy/1, load_candy/2, belt/2]).

read_candy(BeltPid) ->
    Ref = make_ref(),
    BeltPid ! {self(), Ref, read},
    receive
        {Ref, nothing} -> nothing;
        {Ref, {candy, Flavor}} -> Flavor
    end.

load_candy(BeltPid, Flavor) ->
    BeltPid ! {candy, Flavor}.

% The conveyor belt.
% This process models a belt that has:
% - max one candy on it;
% - can inform a RFID reader about the flavour of the candy on the belt;
% - takes the candy to a pusher that decides its fate;
% - accepts a new candy after dropping the last one.
belt(PusherPid, Candy) ->
    receive
        {SenderPid, Ref, read} -> SenderPid ! {Ref, Candy}
    end,
    case Candy of
        {candy, _} -> sr_pusher:push_candy(PusherPid, Candy)
    end,
    receive
        {candy, Flavor} -> belt(PusherPid, {candy, Flavor})
    after
        100 -> belt(PusherPid, nothing)
    end.

