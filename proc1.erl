% src/erlang/proc1.erl   2018-2-13   Alan U. Kennington.
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
