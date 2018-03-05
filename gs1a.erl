% src/erlang/gs1a.erl   2018-3-5   Alan U. Kennington.
% Testing the Erlang/OTP gen_server concept.
% Here the user-interface and server processes are separate, to investigate
% what happens when the advice in the documentation is _not_ followed!
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
% There's something very odd going on here.
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
% It looks any error at all in the erlang shell is causing the gen-server
% loop to drop the gs1b server process.
% This looks at first sight like a bad idea.
% Why should a supposedly stand-alone process collapse when shell errors occur?
%
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%==============================================================================
% Module A.
% This module provides the basic user access functions.
% The gen_server itself is in Module B: gs1b.erl.

-module(gs1a).

% Gen_server: http://erlang.org/doc/man/gen_server.html
% Gen_server: http://erlang.org/doc/design_principles/gen_server_concepts.html
% Behaviours: http://erlang.org/doc/design_principles/des_princ.html#id63247
% Note that this user-interface module does _not_ have gen_server behaviour!!!
% -behaviour(gen_server).

% The main start-up call.
-export([start_link/0]).

% Some services.
-export([alloc/0, free/1]).

% Make the registered name of the server _different_ to the module name.
-define(REG_NAME, gs1reg).

%==============================================================================
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This function is called from the ERL shell.
% So probably the name of the function doesn't matter.
% gen_server:start_link/4 registers the server module gs1b as ?REG_NAME.
% See http://erlang.org/doc/man/gen_server.html#start_link-3
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start_link() ->
    ServerName = {local, ?REG_NAME},
    Module = gs1b,
    Args = [],
    Options = [],
    gen_server:start_link(ServerName, Module, Args, Options).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% User-interface function to call the server via the gen_server module.
% "call" is synchronous.
% See http://erlang.org/doc/man/gen_server.html#call-2
% Return value is the reply from gs1c:alloc/1.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
alloc() ->
    ServerRef = ?REG_NAME,
    Request = alloc,
    gen_server:call(ServerRef, Request).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% User-interface function to call the server via the gen_server module.
% "cast" is asynchronous.
% See http://erlang.org/doc/man/gen_server.html#cast-2
% Return value is "ok" because the return value of gs1c:free/2 is the State.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
free(X) ->
    ServerRef = ?REG_NAME,
    Request = {free, X},
    gen_server:cast(ServerRef, Request).
