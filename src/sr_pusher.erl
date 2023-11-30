-module(sr_pusher).
-export([exclude_next/1, accept_next/1, push_candy/2, init/2]).

% Commands the given pusher to exclude the next candy it receives.
exclude_next(PusherPid) ->
    PusherPid ! exclude_next.

accept_next(PusherPid) ->
    PusherPid ! accept_next.

% Provides a candy to the given pusher
push_candy(PusherPid, {candy, _} = Candy) ->
    PusherPid ! Candy.

init(LogName, Args) ->
    register(pusher_pid, self()),
    {sink, Sink} = proplists:lookup(sink, Args),
    pusher(LogName, Sink).

% Waits for a flavor to exclude (or 'nothing'), then processes a given candy.
pusher(LogName, Sink) ->
    logger:info("[~p] Starting pusher process, awaiting for signal whether to exclude or include next", [LogName]),
    Reject = receive
        exclude_next -> true;
        accept_next -> false
    end,
    logger:info("[~p] The next candy will be excluded? ~p - Awaiting for the next candy", [LogName, Reject]),
    receive
        {candy, _} = Candy ->
            if 
                Reject -> Sink ! {reject, Candy};
                true -> Sink ! {accept, Candy}
            end
    end,
    pusher(LogName, Sink).