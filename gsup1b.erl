% src/erlang/gsup1b.erl   2018-3-7   Alan U. Kennington.
% Testing the Erlang/OTP gen_server concept with a supervisor.

% Module B.
% This is the supervisor "callback module" which contains call handlers.
% Roughly speaking, Module A calls gen_server, which calls Module B.
% The basic command-line functions are in Module A: gsup1a.erl.
% Call chain: Erlang shell <==> SV-A <==> SV-module <- -> SV-daemon <==> SV-B.

-module(gsup1b).

% Behaviours: http://erlang.org/doc/design_principles/des_princ.html#id63247
% Supervisor: http://erlang.org/doc/design_principles/sup_princ.html
% Supervisor: http://erlang.org/doc/man/supervisor.html
% Module attributes: http://erlang.org/doc/reference_manual/modules.html#id78271

-behaviour(supervisor).

% The callback handlers.
-export([init/1]).

% Make the names different so that they can be distinguished.
-define(CHILD_USER_MODULE, gs1a).       % Provides services to Erlang shell.
-define(CHILD_CALLBACK_MODULE, gs1b).   % Provides services to child process.

-define(CHILD_ID_NAME1, gs1id1).        % Child process id.
-define(CHILD_REG_NAME1, gs1reg1).      % Child process registration name.

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
init(_Args) ->
    % strategy() = one_for_all | one_for_one | rest_for_one | simple_one_for_one
    % Intensity :: integer() >= 0
    % Period :: integer() >= 1              [Seconds]
    % The following are the defaults.
    SupFlags = #{ strategy => one_for_one, intensity => 1, period => 5 },

    % The processes to be supervised.
    ChildSpecs = [#{
        id => ?CHILD_ID_NAME1,
        start => { ?CHILD_USER_MODULE, start_link, [?CHILD_REG_NAME1] },
        restart => permanent,           % Default.
%        shutdown => brutal_kill,
        shutdown => 5000,               % Default for a worker.
        type => worker,
        modules => [?CHILD_CALLBACK_MODULE]
        }],
    {ok, {SupFlags, ChildSpecs}}.
