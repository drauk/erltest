# erltest
My Erlang/OTP/wxErlang experiments.

1. **test1.erl**: Some trivial exercises with Erlang functions.

1. **proc1.erl**: Some trivial distributed Erlang processes including a Poisson process.

1. **wx1.erl**: A minimal wxErlang application to see how it works.

1. **mobsim1.erl**: A trivial mobile object simulation using wxErlang.

1. **mobsim2.erl**: Development module for simulation of a simple mobile network.

1. **mobsim3.erl**: Version 3 of mobile network simulation. Added server-side display list, double buffering, window re-painting, multiple options for node appearance, and some trace filtering options.

1. **gs1a.erl**, **gs1b.erl**, **gs1c.erl**: Modules A, B and C to dissect and investigate the gen_server concept.
* The call chain is: Erlang shell <==> A <==> GS-module <--> GS-daemon <==> B <==> C.
  * Erlang shell: ```erl```
  * Module A: ```gs1a.erl```
  * GS-module: ```gen_server.erl```
  * GS-daemon: ```proc_lib.erl``` [the "gen_server process"]
  * Module B: ```gs1b.erl``` [the "callback module"]
  * Module C: ```gs1c.erl```
* Module A converts Erlang shell commands to calls to the GS-module.
* Module B is the "callback module" which handles the GS-daemon's calls.
* Module C provides some basic low-level functions to Module B for demonstration purposes.
* The GS-daemon is called a "gen_server process" in the Erlang/OTP documentation.
* Communication between the GS-module and GS-daemon uses inter-process messages.
* The other links use plain function calls.
* The Erlang/OTP documentation recommends putting A, B and C in a single module.
* But then it is not immediately clear how the whole system works.
