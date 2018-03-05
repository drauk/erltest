% src/erlang/gs1b.erl   2018-3-6   Alan U. Kennington.
% Testing the Erlang/OTP gen_server concept.

% Module B.
% This is the gen_server "callback module" which contains call handlers.
% Roughly speaking, Module A calls gen_server, which calls Module B.
% The basic command-line functions are in Module A: gs1a.erl.
% Call chain: Erlang shell <==> A <==> gen_server <- -> daemon <==> B <==> C.

-module(gs1b).

% Behaviours: http://erlang.org/doc/design_principles/des_princ.html#id63247
% Gen_server: http://erlang.org/doc/design_principles/gen_server_concepts.html
% Gen_server: http://erlang.org/doc/man/gen_server.html
% Module attributes: http://erlang.org/doc/reference_manual/modules.html#id78271

-behaviour(gen_server).

% The callback handlers.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

% According to the "design principles", a good thing about the "client
% functions" in this module is that there is no mention of Module A.
% http://erlang.org/doc/design_principles/des_princ.html#id63247
% So the name can be changed externally without rewriting this module.
% The protocol between gen_server and B is hidden from A.

%==============================================================================
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This is called when this module is being spawned by the gen_server module.
% It is an opportunity to create a single state-structure, which will be
% held by the gen_server module.
% http://erlang.org/doc/man/gen_server.html#Module:init-1
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
init(_Args) ->
    % Trap exit signals.
    % This is supposed to be used only in a supervision tree.
    % http://erlang.org/doc/reference_manual/processes.html#id88677
    % http://erlang.org/doc/design_principles/gen_server_concepts.html#id64119
    %
    % Quote:
    % Receiving Exit Signals
    % The default behaviour when a process receives an exit signal with an exit
    % reason other than normal, is to terminate and in turn emit exit signals
    % with the same exit reason to its linked processes. An exit signal with
    % reason normal is ignored.
    %
    % When a process is trapping exits, it does not terminate when an exit
    % signal is received. Instead, the signal is transformed into a message
    % {'EXIT',FromPid,Reason}, which is put into the mailbox of the process,
    % just like a regular message.
    process_flag(trap_exit, true),

    % The process_flag/2 call is presumably executed in the daemon process,
    % which then traps any exit signal and then calls gs1b:terminate/2.

    InitState = gs1c:channels(),
    { ok, InitState }.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This is the handler for synchronous "call" calls via the gen_server module.
% http://erlang.org/doc/man/gen_server.html#Module:handle_call-3
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handle_call(Request, _From, State) ->
    { X, NewState } = case Request of
        alloc ->
            gs1c:alloc(State);
        _Else ->
            { -1, State }
    end,
    { reply, X, NewState }.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This is the handler for asynchronous "cast" calls via the gen_server module.
% http://erlang.org/doc/man/gen_server.html#Module:handle_cast-2
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handle_cast(Request, State) ->
    NewState = case Request of
        { free, X } ->
            gs1c:free(X, State);
        _Else ->
            State
    end,
    { noreply, NewState }.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This is the handler for "info" calls via the gen_server module.
% http://erlang.org/doc/man/gen_server.html#Module:handle_info-2
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handle_info(Info, State) ->
    case Info of
        { 'EXIT', Pid, Reason } ->
            io:format("Abnormal exit: Pid=~p, Reason=\"~p\"~n", [Pid, Reason]),
            { noreply, State };
        _Else ->
            { noreply, State }
    end.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This is an opportunity to destroy the State in an orderly manner.
% Maybe a message could be sent out to display the terminal State.
% http://erlang.org/doc/design_principles/gen_server_concepts.html#id64119
% http://erlang.org/doc/man/gen_server.html#Module:terminate-2
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
terminate(Reason, _State) ->
    if Reason == normal orelse Reason == shutdown ->
        io:format("gs1b:terminate/2 called for \"~p\" exit~n", [Reason]);
    true ->
        io:format("gs1b:terminate/2 called for ABNORMAL exit~n", [])
    end,
    ok.
