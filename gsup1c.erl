% src/erlang/gsup1c.erl   2018-3-8   Alan U. Kennington.
% Testing the Erlang/OTP gen_server concept with a supervisor.

%==============================================================================
% Supervisor module C.
% This module provides some basic low-level "services" to Modules A and B.
% Call chain:
% Erlang shell <==> SV-A <==> SV-module <- -> SV-daemon <==> SV-B <==> SV-C.
% However, SV-A also invokes functions in SV-C.

-module(gsup1c).

-export([childId/1, childReg/1, childSpec/1, childSpecs/1]).

% Make the names different so that they can be distinguished.
-define(CHILD_USER_MODULE, gs1a).       % Provides services to Erlang shell.
-define(CHILD_CALLBACK_MODULE, gs1b).   % Provides services to child process.

% These definitions should ideally be imported into modules SV-A and SV=B.
-define(CHILD_ID_BASE, gs1id).          % Child process id base.
-define(CHILD_REG_BASE, gs1reg).        % Child process registration name base.

%==============================================================================
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Return an Id for the specified process number.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
childId(Nproc) ->
    % http://erlang.org/doc/man/erlang.html#atom_to_list-1
    % http://erlang.org/doc/man/erlang.html#integer_to_list-1
    % http://erlang.org/doc/man/erlang.html#list_to_atom-1
    list_to_atom(atom_to_list(?CHILD_ID_BASE) ++ integer_to_list(Nproc)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Return a registration name for the specified process number.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
childReg(Nproc) ->
    list_to_atom(atom_to_list(?CHILD_REG_BASE) ++ integer_to_list(Nproc)).

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
    ChildId = childId(Nproc),
    ChildReg = childReg(Nproc),

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
        NewChildSpecs = lists:append(ChildSpecs, [gsup1c:childSpec(Nproc)]),
        childSpecs(Nprocs, Nproc + 1, NewChildSpecs)
    end.
childSpecs(Nprocs) when is_integer(Nprocs) andalso Nprocs >= 0 ->
    childSpecs(Nprocs, 1, []).
