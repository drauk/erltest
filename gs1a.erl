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

% Module A.
% This module provides the basic user access functions.
% The gen_server itself is in Module B: gs1b.erl.
-module(gs1a).

% Gen_server: http://erlang.org/doc/man/gen_server.html
% Gen_server: http://erlang.org/doc/design_principles/gen_server_concepts.html
% Behaviours: http://erlang.org/doc/design_principles/des_princ.html#id63247
% Mod. attributes: http://erlang.org/doc/reference_manual/modules.html#id78271
% Note that this user-interface module does _not_ have gen_server behaviour!!!
% -behaviour(gen_server).

-export([start_link/0]).
-export([alloc/0, free/1]).

% Make the registered name of the server _different_ to the module name.
-define(REG_NAME, gs1reg).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This function is called from the ERL shell.
% So probably the name of the function doesn't matter.
% gen_server:start_link/4 registers the server module gs1b as ?REG_NAME.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start_link() ->
    gen_server:start_link({local, ?REG_NAME}, gs1b, [], []).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% User-interface function to call the server via the gen_server module.
% "call" is synchronous.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
alloc() ->
    gen_server:call(?REG_NAME, alloc).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% User-interface function to call the server via the gen_server module.
% "cast" is asynchronous.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
free(X) ->
    gen_server:cast(?REG_NAME, {free, X}).
