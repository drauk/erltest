# erltest
My Erlang/OTP/wxErlang experiments.

**test1.erl**: Some trivial exercises with Erlang functions.

**proc1.erl**: Some trivial distributed Erlang processes including a Poisson process.

**wx1.erl**: A minimal wxErlang application to see how it works.

**mobsim1.erl**: A trivial mobile object simulation using wxErlang.

**mobsim2.erl**: Development module for simulation of a simple mobile network.

**mobsim3.erl**: Version 3 of mobile network simulation. Added server-side display list, double buffering, window re-painting, multiple options for node appearance, and some trace filtering options.

**gs1a.erl**, **gs1b.erl**, **gs1c.erl**: Modules A, B and C to dissect and investigate the gen_server concept.
* Module A transmits Erlang shell commands to the gen_server module.
* Module B is the call-handler module which the gen_server module calls.
* Module C provides some basic low-level functions to Module B for demonstration purposes.
* The call chain is: Erlang shell <==> A <==> GS-module <--> GS-daemon <==> B <==> C.
* The GS-daemon is called the "gen_server" in the Erlang/OTP documentation.
* Communication between the GS-module and GS-daemon uses inter-process messages.
* The other links use plain function calls.
