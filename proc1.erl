% src/erlang/proc1.erl   2018-2-12   Alan U. Kennington.
% $Id$
% Test run of erlang programming language "processes" (i.e. threads).
% Based on: http://erlang.org/doc/getting_started/conc_prog.html
% For sleep/1: http://erlang.org/doc/man/timer.html#sleep-1
% For is_integer/1: http://erlang.org/doc/man/erlang.html#is_integer-1
% For andalso: http://erlang.org/doc/reference_manual/expressions.html#id82894

-module(proc1).

-export([procstartA/0, procA/2]).

procA(_, 0) ->
    done;
procA(S, Ntimes) when is_integer(Ntimes) andalso Ntimes >= 1 ->
    io:format("~p~n", [S]),
    timer:sleep(1000),
    procA(S, Ntimes - 1).

procstartA() ->
    spawn(proc1, procA, ["process 1", 4]),
    spawn(proc1, procA, ["process 2", 4]).
