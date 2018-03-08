% src/erlang/gsup1b.erl   2018-3-8   Alan U. Kennington.
% Testing the Erlang/OTP gen_server concept with a supervisor.

%==============================================================================
% Supervisor module B.
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

% These definitions must match gsup1a.erl.
-define(CHILD_ID_BASE, gs1id).          % Child process id base.
-define(CHILD_REG_BASE, gs1reg).        % Child process registration name base.

%==============================================================================
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Create one child-process specification.
% http://erlang.org/doc/man/supervisor.html
% Quote:
%   "modules" is used by the release handler during code replacement to
%   determine which processes are using a certain module. As a rule of thumb,
%   if the child process is a "supervisor", "gen_server" or, "gen_statem",
%   this is to be a list with one element "[Module]", where "Module" is the
%   callback module. If the child process is an event manager ("gen_event")
%   with a dynamic set of callback modules, value "dynamic" must be used.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
childSpec(Nproc) when is_integer(Nproc) andalso Nproc >= 1 ->
    % http://erlang.org/doc/man/erlang.html#atom_to_list-1
    % http://erlang.org/doc/man/erlang.html#integer_to_list-1
    % http://erlang.org/doc/man/erlang.html#list_to_atom-1
    ChildId = list_to_atom(atom_to_list(?CHILD_ID_BASE)
            ++ integer_to_list(Nproc)),
    ChildReg = list_to_atom(atom_to_list(?CHILD_REG_BASE)
            ++ integer_to_list(Nproc)),

    % A child process which is to be supervised.
    % http://erlang.org/doc/man/supervisor.html#type-child_spec
    #{
        id => ChildId,
        start => { ?CHILD_USER_MODULE, start_link, [ChildReg] },
        restart => permanent,           % Default.
%        shutdown => brutal_kill,
        shutdown => 5000,               % Default is 5 seconds for a worker.
        type => worker,
        modules => [?CHILD_CALLBACK_MODULE]
    }.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Construct a list of Nprocs child specificiations.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
childSpecs(Nprocs, Nproc, ChildSpecs) when is_list(ChildSpecs)
        andalso is_integer(Nprocs) andalso Nprocs >= 0
        andalso is_integer(Nproc) andalso Nproc >= 1 ->
    if Nproc > Nprocs ->
        ChildSpecs;
    true ->
        % http://erlang.org/doc/man/lists.html#append-2
        NewChildSpecs = lists:append(ChildSpecs, [childSpec(Nproc)]),
        childSpecs(Nprocs, Nproc + 1, NewChildSpecs)
    end.
childSpecs(Nprocs) when is_integer(Nprocs) andalso Nprocs >= 0 ->
    childSpecs(Nprocs, 1, []).

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
    ChildSpecs = childSpecs(Nprocs),
    io:format("gsup1b:init/1: ChildSpecs =~n~p~n", [ChildSpecs]),

    % http://erlang.org/doc/man/supervisor.html#check_childspecs-1
    Check = supervisor:check_childspecs(ChildSpecs),
    io:format("gsup1b:init/1: ChildSpecs check = ~p~n", [Check]),

    {ok, {SupFlags, ChildSpecs}}.
