% src/erlang/gsup1a.erl   2018-3-8   Alan U. Kennington.
% Investigating the Erlang/OTP gen_server concept with a supervisor.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Call chain: Erlang shell <==> SV-A <==> SV-module <- -> SV-daemon <==> SV-B.
% Erlang shell: erl
% Module SV-A:  gsup1a.erl
% SV-module:    supervisor.erl
% SV-daemon:    proc_lib.erl        [the "gen_server" process]
% Module SV-B:  gsup1b.erl          [the "callback module"]
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The supervisor:start_link/3 function starts a daemon process which
% has a receive-loop which waits for messages sent to it.
% See http://erlang.org/doc/man/supervisor.html
% The start_link functions are actually in proc_lib.erl.
% See http://erlang.org/doc/man/proc_lib.html#start-3
% The proc_lib.erl functions spawn processes using the spawn BIFs.
% http://erlang.org/doc/man/erlang.html#spawn-1
% http://erlang.org/doc/man/erlang.html#spawn_link-1
% http://erlang.org/doc/man/erlang.html#spawn_monitor-1
% http://erlang.org/doc/man/erlang.html#spawn_opt-2
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The "SV-module <- -> SV-daemon" link uses inter-process messaging.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Test: [It worked the first time!!!]
% 1> gsup1a:start_link().
% {ok,<0.62.0>}
% 2> gs1a:alloc(gs1reg1).
% 1
% 3> i().
% [....]
%
% <0.62.0>              supervisor:gsup1b/1                    233      166    0
% gsup1reg              gen_server:loop/7                       10
% <0.63.0>              gs1b:init/1                            233      131    0
% gs1reg1               gen_server:loop/7                       10
% [....]
% ok
% 4> gs1a:alloc(gs1reg1).
% 2
% 5> supervisor:which_children(gsup1reg).
% [{gs1id1,<0.63.0>,worker,[gs1b]}]
% 6> supervisor:count_children(gsup1reg).
% [{specs,1},{active,1},{supervisors,0},{workers,1}]
% 7> supervisor:restart_child(gsup1reg, gs1id1).
% {error,running}
% 8> supervisor:terminate_child(gsup1reg, gs1id1).
% gs1b:terminate/2 called for "shutdown" exit
% ok
% 9> supervisor:restart_child(gsup1reg, gs1id1).
% {ok,<0.72.0>}
% 10> gs1a:alloc(gs1reg1).
% 1
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Test with 10 children.
% 1> gsup1a:start_link(10).
% gsup1b:init/1: Nprocs = 10
% gsup1b:init/1: ChildSpecs = [#{type => worker,
%                                start => {gs1a,start_link,[gs1reg1]},
%                                shutdown => 5000,restart => permanent,
%                                modules => [gs1b],
%                                id => gs1id1},
%                              #{type => worker,
%                                start => {gs1a,start_link,[gs1reg2]},
%                                shutdown => 5000,restart => permanent,
%                                modules => [gs1b],
%                                id => gs1id2},
% [....]
%                              #{type => worker,
%                                start => {gs1a,start_link,[gs1reg10]},
%                                shutdown => 5000,restart => permanent,
%                                modules => [gs1b],
%                                id => gs1id10}]
% {ok,<0.62.0>}
% 2> i().
% [....]
%
% <0.62.0>              supervisor:gsup1b/1                    987     1142    0
% gsup1reg              gen_server:loop/7                       10
% <0.63.0>              gs1b:init/1                            233      119    0
% gs1reg1               gen_server:loop/7                       10
% <0.64.0>              gs1b:init/1                            233      101    0
% gs1reg2               gen_server:loop/7                       10
% <0.65.0>              gs1b:init/1                            233      101    0
% gs1reg3               gen_server:loop/7                       10
% <0.66.0>              gs1b:init/1                            233      101    0
% gs1reg4               gen_server:loop/7                       10
% <0.67.0>              gs1b:init/1                            233      101    0
% gs1reg5               gen_server:loop/7                       10
% <0.68.0>              gs1b:init/1                            233      101    0
% gs1reg6               gen_server:loop/7                       10
% <0.69.0>              gs1b:init/1                            233      101    0
% gs1reg7               gen_server:loop/7                       10
% <0.70.0>              gs1b:init/1                            233      101    0
% gs1reg8               gen_server:loop/7                       10
% <0.71.0>              gs1b:init/1                            233      101    0
% gs1reg9               gen_server:loop/7                       10
% <0.72.0>              gs1b:init/1                            233      101    0
% gs1reg10              gen_server:loop/7                       10
% [....]
% ok
% 3> supervisor:which_children(gsup1reg).
% [{gs1id10,<0.72.0>,worker,[gs1b]},
%  {gs1id9,<0.71.0>,worker,[gs1b]},
%  {gs1id8,<0.70.0>,worker,[gs1b]},
%  {gs1id7,<0.69.0>,worker,[gs1b]},
%  {gs1id6,<0.68.0>,worker,[gs1b]},
%  {gs1id5,<0.67.0>,worker,[gs1b]},
%  {gs1id4,<0.66.0>,worker,[gs1b]},
%  {gs1id3,<0.65.0>,worker,[gs1b]},
%  {gs1id2,<0.64.0>,worker,[gs1b]},
%  {gs1id1,<0.63.0>,worker,[gs1b]}]
% 4> supervisor:count_children(gsup1reg).
% [{specs,10},{active,10},{supervisors,0},{workers,10}]
% 5> gs1a:alloc(gs1reg1).
% 1
% 6> gs1a:alloc(gs1reg1).
% 2
% 7> gs1a:alloc(gs1reg9).
% 1
% 8> gs1a:alloc(gs1reg1).
% 3
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%==============================================================================
% Supervisor module A.
% This module provides the basic user access functions to the Erlang shell.

-module(gsup1a).

% The main start-up call.
-export([start_link/0, start_link/1]).

% Some more services for the Erlang shell.
% -export([   ]).

% Make the registered name of the server _different_ to the module name.
-define(SUPER_REG_NAME, gsup1reg).      % The daemon process registration name.
-define(SUPER_SERVICE_MODULE, gsup1b).  % Provides services to the daemon.

%==============================================================================
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This function is called from the Erlang shell.
% So the name of this function doesn't matter.
% supervisor:start_link/3 registers the service module as ?SUPER_REG_NAME.
% See http://erlang.org/doc/man/supervisor.html#start_link-2
% Use start_link for supervised processes. Use start for standalone processes.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start_link(Nprocs) when is_integer(Nprocs) andalso Nprocs >= 0 ->
    SupName = {local, ?SUPER_REG_NAME},
    Module = ?SUPER_SERVICE_MODULE,
    Args = #{ nProcs => Nprocs },       % Passed to Module:init/1.
    supervisor:start_link(SupName, Module, Args).
start_link() ->
    start_link(1).
