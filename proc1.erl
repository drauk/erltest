% src/erlang/proc1.erl   2018-2-17   Alan U. Kennington.
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
-export([startDserver/0, startDclient/1, procDserver/1, procDclient/3]).
-export([poissonE/1, poissonE/2, procEpoisson/2,
    startEpoisson/2, startEpoisson/3]).

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
            io:format("procCserver ~p sleep ~p~n", [self(), Tsleep]),
            timer:sleep(Tsleep),
            io:format("procCserver ~p sending response to ~p [~p]~n",
                [self(), PIDclient, NtimesRX]),
            PIDclient ! { resp, self(), NtimesRX },
            io:format("procCserver ~p sleep ~p~n", [self(), Tsleep]),
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
    io:format("procCclient ~p sleep ~p~n", [self(), Tsleep]),
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

%==============================================================================
% Example D. Two registered processes with some basic message passing.
% Test:
% In window 1.
% erl -sname serverD
% (serverD@hostA)1> c(proc1).
% {ok,proc1}
% (serverD@hostA)2> proc1:startDserver().
% procDserver <0.79.0> waiting
% true
%
% In window 2.
% erl -sname clientD
% (clientD@hostA)1> c(proc1).
% {ok,proc1}
% (clientD@hostA)2> proc1:startDclient(serverD@hostA).
% procDclient <0.115.0> sending msg 5 to serverD@hostA
% <0.115.0>
% ....
%
% Note: If you modify the source, you must recompile in _both_ windows!!!
% At least that's true if you don't restart the "erl" Unix-processes.
%
% In theory, you can run the server and client on two different hosts.
% You must open TCP port 4369 so that epmd can signal port numbers.
% However, the erl processes use dynamically allocated TCP port numbers.
% So you have to effectively open all TCP ports in the firewall.
% Then you can get communication going between erl processes on different hosts.
% This seems extremely unwise to me, opening up all TCP ports for Erlang.
% Then your entire systems will be open to every kind of hacking.
%
% To find out what ports are being used, run "epmd -names". Example:
% user@hostA> epmd -names
% epmd: up and running on port 4369 with data:
% name clientD at port 55366
% name serverD at port 37248
%
% If you create 2 client processes, and then call
% proc1:startDclient(serverD@hostA) at about the same time,
% they work as expected, because responses to to the correct senders.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The server.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procDserver(Tsleep) ->
    io:format("procDserver ~p waiting~n", [self()]),
    receive
        { msg, PIDclient, NtimesRX } ->
            io:format("procDserver ~p received msg ~p from ~p~n",
                [self(), NtimesRX, PIDclient]),
            io:format("procDserver ~p sleep ~p~n", [self(), Tsleep]),
            timer:sleep(Tsleep),
            io:format("procDserver ~p sending response to ~p [~p]~n",
                [self(), PIDclient, NtimesRX]),
            PIDclient ! { resp, self(), NtimesRX },
            io:format("procDserver ~p sleep ~p~n", [self(), Tsleep]),
            procDserver(Tsleep);
        { fin, PIDclient, NtimesRX } ->
            io:format("procDserver ~p received fin ~p from ~p~n",
                [self(), NtimesRX, PIDclient]),
            io:format("procDserver ~p END~n", [self()])
    end.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The client.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procDclient(Ntimes, PIDserver, Tsleep) when Ntimes > 0 ->
    io:format("procDclient ~p sending msg ~p to ~p~n",
        [self(), Ntimes, PIDserver]),
    { pidDserver, PIDserver } ! { msg, self(), Ntimes },
    io:format("procDclient ~p waiting for response [~p]~n", [self(), Ntimes]),
    receive
        { resp, PIDserverRX, NtimesRX } ->
            io:format("procDclient ~p received response ~p from ~p~n",
                [self(), NtimesRX, PIDserverRX])
    end,
    io:format("procDclient ~p sleep ~p~n", [self(), Tsleep]),
    timer:sleep(Tsleep),
    procDclient(Ntimes - 1, PIDserver, Tsleep);
procDclient(Ntimes, PIDserver, _) when Ntimes =< 0 ->
    io:format("procDclient ~p sending fin to server ~p [~p]~n",
        [self(), PIDserver, Ntimes]),
    { pidDserver, PIDserver } ! { fin, self(), Ntimes },
    io:format("procDclient ~p END~n", [self()]).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The network creation.
% When using "register/2", must use lower case because the name of the
% process is now a label, not a variable.
% If the process pidDserver has not died for some reason, the label cannot
% be re-used.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startDserver() ->
    register(pidDserver, spawn(proc1, procDserver, [1000])).
startDclient(PIDserver) ->
    spawn(proc1, procDclient, [5, PIDserver, 2500]).

%==============================================================================
% Example E. Poisson process.
% Test:
% >>> proc1:poissonE(10).
% 16.87000453459495
% >>> proc1:poissonE(10).
% 1.707294812752144
%
% >>> proc1:poissonE(10, 100).
% 9.679999978534767
% >>> proc1:poissonE(10, 100).
% 10.291440498812618
%
% >>> proc1:poissonE(5, 100).
% 5.557374407497482
% >>> proc1:poissonE(5, 100).
% 5.139241650823554
% >>> proc1:poissonE(5, 100).
% 4.925384013290105

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% poissonE/1. Poisson distribution t -> exp(-Kt), K = 1/Mu. Mean Mu.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
poissonE(Mu) when is_number(Mu) andalso Mu >= 0 ->
    X = rand:uniform(),
    -math:log(1.0 - X) * Mu.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% poissonE/4. Add up Ntotal values and average them.
% This is a really crazy way to average Ntotal numbers!
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
poissonE(Mu, Ntotal, N, Sum) when is_number(Mu) andalso Mu >= 0
        andalso is_integer(Ntotal) andalso Ntotal >= 1
        andalso is_integer(N) andalso N > 1 ->
    X = poissonE(Mu),
    poissonE(Mu, Ntotal, N - 1, Sum + X);
poissonE(Mu, Ntotal, 1, Sum) when is_number(Mu) andalso Mu >= 0
        andalso is_integer(Ntotal) andalso Ntotal >= 1 ->
    X = poissonE(Mu),
    (Sum + X) / Ntotal.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% poissonE/2. Poisson distribution. Average of Ntotal values, mean Mu.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
poissonE(Mu, Ntotal) when is_number(Mu) andalso Mu >= 0
        andalso is_integer(Ntotal) andalso Ntotal >= 1 ->
    poissonE(Mu, Ntotal, Ntotal, 0.0).

%------------------------------------------------------------------------------
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% procEpoisson/4. Add up Ntotal values and average them.
% The math:floor/1, math:ceil/1, math:round/1 functions output float.
% The erlang BIFs floor/1, ceil/1, round/1 output integer.
% And timer:sleep/1 requires integer!
%
% Test:
% >>> proc1:procEpoisson(5, 10).
% procEpoisson <0.274.0>:
%  N = 10, X = 7.163964130317594, Tsleep = 7164, Sum = 7.163964130317594
% procEpoisson <0.274.0>:
%  N = 9, X = 1.3160921312371316, Tsleep = 1316, Sum = 8.480056261554726
% procEpoisson <0.274.0>:
%  N = 8, X = 4.518958787613862, Tsleep = 4519, Sum = 12.999015049168587
% procEpoisson <0.274.0>:
%  N = 7, X = 4.873551884156486, Tsleep = 4874, Sum = 17.872566933325075
% procEpoisson <0.274.0>:
%  N = 6, X = 0.4557487720474376, Tsleep = 456, Sum = 18.328315705372514
% procEpoisson <0.274.0>:
%  N = 5, X = 4.9034309060690155, Tsleep = 4903, Sum = 23.23174661144153
% procEpoisson <0.274.0>:
%  N = 4, X = 0.17451090887489748, Tsleep = 175, Sum = 23.406257520316426
% procEpoisson <0.274.0>:
%  N = 3, X = 5.725110242929824, Tsleep = 5725, Sum = 29.13136776324625
% procEpoisson <0.274.0>:
%  N = 2, X = 21.57473359926596, Tsleep = 21575, Sum = 50.706101362512214
% procEpoisson <0.274.0>:
%  N = 1, X = 12.8591281344789, Tsleep = 12859, Sum = 63.565229496991115
% procEpoisson <0.274.0>: avg=6.356522949699111 END
% ok
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procEpoisson(Mu, Ntotal, N, Sum) when is_number(Mu) andalso Mu >= 0
        andalso is_integer(Ntotal) andalso Ntotal >= 1
        andalso is_integer(N) andalso N > 1 ->
    X = poissonE(Mu),
    Tsleep = floor(1000 * X + 0.5),
    NewSum = Sum + X,
    io:format("procEpoisson ~p:~n N = ~p, X = ~p, Tsleep = ~p, Sum = ~p~n",
        [self(), N, X, Tsleep, NewSum]),
    timer:sleep(Tsleep),
    procEpoisson(Mu, Ntotal, N - 1, NewSum);
procEpoisson(Mu, Ntotal, 1, Sum) when is_number(Mu) andalso Mu >= 0
        andalso is_integer(Ntotal) andalso Ntotal >= 1 ->
    X = poissonE(Mu),
    Tsleep = floor(1000 * X + 0.5),
    NewSum = Sum + X,
    io:format("procEpoisson ~p:~n N = ~p, X = ~p, Tsleep = ~p, Sum = ~p~n",
        [self(), 1, X, Tsleep, NewSum]),
    timer:sleep(Tsleep),
    io:format("procEpoisson ~p: avg=~p END~n", [self(), NewSum / Ntotal]).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% procEpoisson/2. Poisson process. Average of Ntotal values, mean Mu.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procEpoisson(Mu, Ntotal) when is_number(Mu) andalso Mu >= 0
        andalso is_integer(Ntotal) andalso Ntotal >= 1 ->
    procEpoisson(Mu, Ntotal, Ntotal, 0.0).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Spawn a procEpoisson/2 process.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startEpoisson(Mu, Ntotal) when is_number(Mu) andalso Mu >= 0
        andalso is_integer(Ntotal) andalso Ntotal >= 1 ->
    spawn(proc1, procEpoisson, [Mu, Ntotal]).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Spawn many procEpoisson/2 processes.
% For exit: http://erlang.org/doc/reference_manual/errors.html#id87812
% Exit handling: http://erlang.org/doc/reference_manual/processes.html#id88677
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startEpoisson(Mu, Ntotal, Nproc) when is_number(Mu) andalso Mu >= 0
        andalso is_integer(Ntotal) andalso Ntotal >= 1
        andalso is_integer(Nproc) andalso Nproc >= 0 ->
    if
        Nproc =< 0 ->
            io:format("startEpoisson ~p finished~n", [self()]),
            exit(normal);
        true ->
            true
    end,
    io:format("startEpoisson ~p spawning process ~p~n", [self(), Nproc]),
    spawn(proc1, procEpoisson, [Mu, Ntotal]),
    startEpoisson(Mu, Ntotal, Nproc - 1).
