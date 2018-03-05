% src/erlang/gs1b.erl   2018-3-5   Alan U. Kennington.
% Testing the Erlang/OTP gen_server concept.

% Module B.
% This is the gen_server "callback module" which contains call handlers.
% Roughly speaking, Module A calls gen_server, which calls Module B.
% The basic command-line functions are in Module A: gs1a.erl.

-module(gs1b).

% Behaviours: http://erlang.org/doc/design_principles/des_princ.html#id63247
% Gen_server: http://erlang.org/doc/design_principles/gen_server_concepts.html
% Gen_server: http://erlang.org/doc/man/gen_server.html
% Module attributes: http://erlang.org/doc/reference_manual/modules.html#id78271

-behaviour(gen_server).

% The callback handlers.
-export([init/1, handle_call/3, handle_cast/2]).

% According to the "design principles", a good thing about the "client
% functions" in this module is that there is no mention of Module A.
% http://erlang.org/doc/design_principles/des_princ.html#id63247
% So the name can be changed externally without rewriting this module.
% The communication chain is: A <--> gen_server <--> B <--> C.
% The protocol between gen_server and B is hidden from A.

%==============================================================================
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This is called when this module is being spawned by the gen_server module.
% It is an opportunity to create a single state-structure, which will be
% held by the gen_server module.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
init(_Args) ->
    { ok, gs1c:channels() }.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This is the handler for synchronous "call" calls via the gen_server module.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handle_call(alloc, _From, State) ->
    { X, NewState } = gs1c:alloc(State),
    { reply, X, NewState }.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This is the handler for ssynchronous "cast" calls via the gen_server module.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handle_cast({ free, X }, State) ->
    NewState = gs1c:free(X, State),
    { noreply, NewState }.
