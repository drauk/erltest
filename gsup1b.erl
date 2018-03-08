% src/erlang/gsup1b.erl   2018-3-8   Alan U. Kennington.
% Testing the Erlang/OTP gen_server concept with a supervisor.

%==============================================================================
% Supervisor module B.
% This is the supervisor "callback module" which contains call handlers.
% Roughly speaking, Module A calls gen_server, which calls Module B.
% The basic command-line functions are in Module A: gsup1a.erl.
% Call chain:
% Erlang shell <==> SV-A <==> SV-module <- -> SV-daemon <==> SV-B <==> SV-C.
% However, SV-A also invokes functions in SV-C.

-module(gsup1b).

% Behaviours: http://erlang.org/doc/design_principles/des_princ.html#id63247
% Supervisor: http://erlang.org/doc/design_principles/sup_princ.html
% Supervisor: http://erlang.org/doc/man/supervisor.html
% Module attributes: http://erlang.org/doc/reference_manual/modules.html#id78271

-behaviour(supervisor).

% The callback handlers.
-export([init/1]).

%==============================================================================
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This is called when this module is being spawned by the gen_server module.
% It is an opportunity to create a single state-structure, which will be
% held by the gen_server module.
% Module:init/1: http://erlang.org/doc/man/supervisor.html#Module:init-1
% SupFlags: http://erlang.org/doc/design_principles/sup_princ.html#id79178
% SupFlags: http://erlang.org/doc/man/supervisor.html#type-sup_flags
% Strategy: http://erlang.org/doc/design_principles/sup_princ.html#id79244
% Strategy: http://erlang.org/doc/man/supervisor.html#type-strategy
% ChildSpec: http://erlang.org/doc/design_principles/sup_princ.html#id79540
% ChildSpec: http://erlang.org/doc/man/supervisor.html#type-child_spec
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
init(Args) when is_map(Args) ->
    NprocsDefault = 1,
    Nprocs = maps:get(nProcs, Args, NprocsDefault),
    io:format("gsup1b:init/1: Nprocs = ~p~n", [Nprocs]),

    % strategy() = one_for_all | one_for_one | rest_for_one | simple_one_for_one
    % Intensity :: integer() >= 0           [default = 1]
    % Period :: integer() >= 1              [Seconds, default = 5]
    % The following are the defaults.
    SupFlags = #{ strategy => one_for_one, intensity => 1, period => 5 },

    % The processes to be supervised.
    ChildSpecs = gsup1c:childSpecs(Nprocs),
    io:format("gsup1b:init/1: ChildSpecs =~n~p~n", [ChildSpecs]),

    % http://erlang.org/doc/man/supervisor.html#check_childspecs-1
    Check = supervisor:check_childspecs(ChildSpecs),
    io:format("gsup1b:init/1: ChildSpecs check = ~p~n", [Check]),

    {ok, {SupFlags, ChildSpecs}}.
