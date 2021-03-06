% src/erlang/gs1a.erl   2018-3-9   Alan U. Kennington.
% Investigating the Erlang/OTP gen_server concept.
% Here the user-interface and service modules are separate, to find out
% what happens when the advice in the documentation is _not_ followed!
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Call chain: Erlang shell <==> A <==> GS-module <- -> GS-daemon <==> B <==> C.
% Erlang shell: erl
% Module A:     gs1a.erl
% GS-module:    gen_server.erl
% GS-daemon:    proc_lib.erl        [the "gen_server" process]
% Module B:     gs1b.erl            [the "callback module"]
% Module C:     gs1c.erl
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The gen_server:start_link/4 function starts a daemon process which
% has a receive-loop which waits for messages sent to it by the
% gen_server:call/2 and gen_server:cast/2 functions.
% The start and start_link functions are actually in proc_lib.erl.
% See http://erlang.org/doc/man/proc_lib.html#start-3
% The proc_lib.erl functions spawn processes using the spawn BIFs.
% http://erlang.org/doc/man/erlang.html#spawn-1
% http://erlang.org/doc/man/erlang.html#spawn_link-1
% http://erlang.org/doc/man/erlang.html#spawn_monitor-1
% http://erlang.org/doc/man/erlang.html#spawn_opt-2
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The gen_server:call/2 function waits until a response is received from
% "client functions" in Module B.
% The "GS-module <- -> GS-daemon" uses inter-process messaging.
% The documentation calls the daemon a "gen_server process".
% See http://erlang.org/doc/design_principles/gen_server_concepts.html#id60670
% See http://erlang.org/doc/man/gen_server.html
%==============================================================================
% Module A.
% This module provides the basic user access functions.
% The services provided via gen_server are in Module B: gs1b.erl.

-module(gs1a).
-vsn(1).

% Note that this user-interface module does _not_ have gen_server behaviour!!!
% Gen_server: http://erlang.org/doc/man/gen_server.html
% Gen_server: http://erlang.org/doc/design_principles/gen_server_concepts.html
% Behaviours: http://erlang.org/doc/design_principles/des_princ.html#id63247
% -behaviour(gen_server).

% The main start-up call.
-export([start_link/0, start_link/1]).

% Some services for the Erlang shell.
-export([alloc/0, alloc/1, free/1, free/2, stop/0, stop/1]).

% Make the registered name of the server _different_ to the module name.
-define(SERVER_REG_NAME, gs1reg).       % The daemon process registration name.
-define(SERVICE_MODULE, gs1b).          % Provides services to the daemon.

%==============================================================================
% To test modules gs1a, gs1b and gs1c:
%
% 1> c(gs1a).
% {ok,gs1a}
% 2> c(gs1b).
% {ok,gs1b}
% 3> c(gs1c).
% {ok,gs1c}
%
% 4> gs1a:start_link().
% {ok,<0.63.0>}
% 5> gs1a:alloc().
% 1
% 6> gs1a:free(1).
% ok
%
% 7> i(0,63,0).
% [{registered_name,gs1reg},
%  {current_function,{gen_server,loop,7}},
% [....]
%
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% There's something odd going on here.
%
% 1> gs1a:start_link().
% {ok,<0.62.0>}
% 2> i().
% [....]
% <0.62.0>              gs1b:init/1                            233      118    0
% gs1reg                gen_server:loop/7                       10
%
% ok
% 3> gs1a:alloc().
% 1
% 2> i().
% [....]
% <0.62.0>              gs1b:init/1                            233      130    0
% gs1reg                gen_server:loop/7                       10
% [....]
%
% ok
% 5> gs1a:xyz().
% ** exception error: undefined function gs1a:xyz/0
% 6> i().
% [....]
% <0.67.0>              erlang:apply/2                        2586    27930    0
%                       c:pinfo/1                               50
% 7> gs1a:alloc().
% ** exception exit: {noproc,{gen_server,call,[gs1reg,alloc]}}
%      in function  gen_server:call/2 (gen_server.erl, line 210)
% 8> gs1a:start_link().
% {ok,<0.72.0>}
% 9> gs1a:alloc().
% 1
%
% When a single incorrect call is made to module gs1a, the process gs1reg
% is apparently terminated by the gen_server module.
% Then it can be restarted again.
% But it is not obvious why a running instance of gs1b should be terminated
% when an incorrect call is made to a non-existent function in gs1a.
%
% This seems to give a clue.
%
% 10> xyz:abc().
% ** exception error: undefined function xyz:abc/0
% 11> gs1a:alloc().
% ** exception exit: {noproc,{gen_server,call,[gs1reg,alloc]}}
%      in function  gen_server:call/2 (gen_server.erl, line 210)
%
% It looks like any error at all in the erlang shell is causing the gen-server
% loop to drop the gs1b server process.
% This looks at first sight like a bad idea.
% Why should a supposedly stand-alone process collapse when shell errors occur?
%
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%==============================================================================
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This function is called from the Erlang shell or by a supervisor process.
% So probably the name of the function doesn't matter.
% gen_server:start_link/4 registers the service module as ?SERVER_REG_NAME.
% See http://erlang.org/doc/man/gen_server.html#start_link-3
% Use start_link for supervised processes. Use start for standalone processes.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start_link(ServerRegName) ->
    ServerName = {local, ServerRegName},
    Module = ?SERVICE_MODULE,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).
start_link() ->
    start_link(?SERVER_REG_NAME).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% User-interface function to call the server via the gen_server module.
% "call" is synchronous.
% See http://erlang.org/doc/man/gen_server.html#call-2
% Return value is the reply from gs1c:alloc/1.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
alloc(ServerRegName) ->
    ServerRef = ServerRegName,
    Request = alloc,
    gen_server:call(ServerRef, Request).
alloc() ->
    alloc(?SERVER_REG_NAME).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% User-interface function to call the server via the gen_server module.
% "cast" is asynchronous.
% See http://erlang.org/doc/man/gen_server.html#cast-2
% Return value is "ok" because the return value of gs1c:free/2 is the State.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
free(ServerRegName, X) ->
    ServerRef = ServerRegName,
    Request = {free, X},
    gen_server:cast(ServerRef, Request).
free(X) ->
    free(?SERVER_REG_NAME, X).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Ask the gen_server module to stop the daemon normally.
% http://erlang.org/doc/man/gen_server.html#stop-1
% Can also do "stop(ServerRef, Reason, Timeout) -> ok".
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stop(ServerRegName) ->
    gen_server:stop(ServerRegName).
stop() ->
    stop(?SERVER_REG_NAME).
