% src/erlang/proc1.erl   2018-2-14   Alan U. Kennington.
% $Id$
% Test run of erlang programming language "processes" (i.e. threads).
% Based on: http://erlang.org/doc/getting_started/conc_prog.html
% For sleep/1: http://erlang.org/doc/man/timer.html#sleep-1
% For is_integer/1: http://erlang.org/doc/man/erlang.html#is_integer-1
% For andalso: http://erlang.org/doc/reference_manual/expressions.html#id82894
% For spawn/3: http://erlang.org/doc/man/erlang.html#spawn-3

-module(proc1).

-export([procstartA/0, procstartAx/0, procA/2, procA/3]).
-export([procstartB/0, procBserver/1, procBclient/3]).
-export([procstartC/0, procCserver/1, procCclient/3]).

%==============================================================================
% Example A. Two independent processes with no message passing.
% Test:
% >>> proc1:procstartA().
% Test:
% >>> proc1:procstartAx().
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% 2-parameter version.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procA(S, 0) ->
    io:format("~p~n", [S]),
    done;
procA(S, Ntimes) when is_integer(Ntimes) andalso Ntimes >= 1 ->
    io:format("~p~n", [S]),
    timer:sleep(1000),
    procA(S, Ntimes - 1).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% 3-parameter version.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procA(S, 0, _) ->
    io:format("~p ~p~n", [S, 0]),
    done;
procA(S, Ntimes, Tsleep) when is_integer(Ntimes) andalso Ntimes >= 1
        andalso is_number(Tsleep) andalso Tsleep >= 0 ->
    io:format("~p ~p~n", [S, Ntimes]),
    timer:sleep(Tsleep),
    procA(S, Ntimes - 1, Tsleep).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Erlang automatically works out the arity of procA !!
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procstartA() ->
    spawn(proc1, procA, ["process 1", 4]),
    spawn(proc1, procA, ["process 2", 4]).
procstartAx() ->
    spawn(proc1, procA, ["process 1000", 10, 1000]),
    spawn(proc1, procA, ["process 2500", 10, 2500]).

%==============================================================================
% Example B. Two processes with some basic message passing.
% Test:
% >>> proc1:procstartB().
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The server.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procBserver(Tsleep) ->
    io:format("~p~n", ["procBserver waiting"]),
    receive
        fin ->
            io:format("~p~n", ["procBserver received finish command"]);
        { msg, PIDclient } ->
            io:format("~p~n", ["procBserver received msg command"]),
            timer:sleep(Tsleep),
            PIDclient ! resp,
            procBserver(Tsleep)
    end.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The client.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procBclient(Ntimes, PIDserver, Tsleep) when Ntimes > 0 ->
    io:format("~p ~p~n", ["procBclient sending msg", Ntimes]),
    PIDserver ! { msg, self() },
    io:format("~p ~p~n", ["procBclient waiting", Ntimes]),
    receive
        resp ->
            io:format("~p ~p~n", ["procBclient received response", Ntimes])
    end,
    timer:sleep(Tsleep),
    procBclient(Ntimes - 1, PIDserver, Tsleep);
procBclient(Ntimes, PIDserver, _) when Ntimes =< 0 ->
    io:format("~p ~p~n", ["procBclient finishing", Ntimes]),
    PIDserver ! fin.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The network creation.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procstartB() ->
    PIDserver = spawn(proc1, procBserver, [1000]),
    spawn(proc1, procBclient, [5, PIDserver, 2500]).

%==============================================================================
% Example C. Two registered processes with some basic message passing.
% Test:
% >>> proc1:procstartC().
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The server.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procCserver(Tsleep) ->
    io:format("procCserver ~p waiting~n", [self()]),
    receive
        { msg, PIDclient, NtimesRX } ->
            io:format("procCserver ~p received msg ~p from ~p~n",
                [self(), NtimesRX, PIDclient]),
            timer:sleep(Tsleep),
            io:format("procCserver ~p sending response to ~p [~p]~n",
                [self(), PIDclient, NtimesRX]),
            PIDclient ! { resp, self(), NtimesRX },
            procCserver(Tsleep);
        { fin, PIDclient, NtimesRX } ->
            io:format("procCserver ~p received fin ~p from ~p~n",
                [self(), NtimesRX, PIDclient]),
            io:format("procCserver ~p END~n", [self()])
    end.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The client.
% In this case, the parameter "PIDserver" is the label "pidCserver",
% not a raw process ID.
% But the value of self() in procCserver/1 is the raw process ID.
% This makes it difficult to identify labels with raw process IDs.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procCclient(Ntimes, PIDserver, Tsleep) when Ntimes > 0 ->
    io:format("procCclient ~p sending msg ~p to ~p~n",
        [self(), Ntimes, PIDserver]),
    PIDserver ! { msg, self(), Ntimes },
    io:format("procCclient ~p waiting for response [~p]~n", [self(), Ntimes]),
    receive
        { resp, PIDserverRX, NtimesRX } ->
            io:format("procCclient ~p received response ~p from ~p~n",
                [self(), NtimesRX, PIDserverRX])
    end,
    timer:sleep(Tsleep),
    procCclient(Ntimes - 1, PIDserver, Tsleep);
procCclient(Ntimes, PIDserver, _) when Ntimes =< 0 ->
    io:format("procCclient ~p sending fin to server ~p [~p]~n",
        [self(), PIDserver, Ntimes]),
    PIDserver ! { fin, self(), Ntimes },
    io:format("procCclient ~p END~n", [self()]).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The network creation.
% When using "register/2", must use lower case because the name of the
% process is now a label, not a variable.
% If the process pidCserver has not died for some reason, the label cannot
% be re-used.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procstartC() ->
    register(pidCserver, spawn(proc1, procCserver, [1000])),
    spawn(proc1, procCclient, [5, pidCserver, 2500]).
