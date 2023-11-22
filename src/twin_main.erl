-module(twin_main).
-export([init/1]).

init(_) ->
    {Sink, TwinSink} = twin_env:spawn_twin(sr_sink, sink, [], []),
    {Pusher, TwinPusher} = twin_env:spawn_twin(sr_pusher, pusher, [Sink], [TwinSink]),
    {Pusher, TwinPusher}.