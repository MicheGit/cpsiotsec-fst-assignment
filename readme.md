# How to run the environment

To start the system, first open an erlang shell on the node twin, like this:

```Shell
erl -sname twin@localhost
```

and also to the mitm node, like this:

```Shell
erl -sname mitm@localhost
```

Then, run `start.sh` (in linux)

```Shell
sh ./start.sh
```

or its content in Windows:

```
erl -make
erl -sname main@localhost -config sr.config -pa ebin/
```

Now the program will create the main processes on the current node (called `main`),
the mitm process on the `mitm` node and the twin processes on the `twin` node.

Finally, from the `main` shell, run this command, which starts the simulation:

```Erlang
application:start(sr).
```

This command will simulate a belt carrying candy of various flavours (except for lemon), 
with the system ready to exclude one lemon candy for simulation cycle.

## Simulating MITM attacks

Go to the previously opened shell `mitm@localhost`. Run this erlang command:

```Erlang
mitm_pid ! {inject_packet, {candy, lemon}}.
```

This command will overwrite the next candy read from the RFID reader, causing a non-lemon candy
to be rejected. This will be detected by the system and the result can be seen 
in the log file located in `log/sr.log`.

## Simulating code replacement from an internal

To simulate code replacement, start the environment as said above.

Then, from the command line of `main@localhost` tell the Erlang runtime to replace
the code of the running PLC process with a malicious one:

```ErlangShell
> c("malicious/sr_plc.erl").
```

This command will replace the PLC process to a malicious one, which causes to
reject all the candies regardless of the flavour. The system will successfully detect the
anomaly.


