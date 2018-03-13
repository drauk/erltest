% src/erlang/mobsim4d.erl   2018-3-14   Alan U. Kennington.
% This module simulates a toy mobile network using wxErlang and wx_object.
% Module A is the UI module.
% Module B is the server module.
% Module C is the window server functions module.
% Module D is the mobile network functions module.

-module(mobsim4d).

% Client process.
-export([startMobileB/1, startMobileBq/1,
    startMobileB/4, startMobileBq/4,
    procMobSimB/5,  % Exported for spawning.
    startMobileBsample1/1, startMobileBsample1q/1,
    startMobileBsample2/1, startMobileBsample2q/1,
    startMobileBsample3/1, startMobileBsample3q/1,
    startMobileBsample4/1, startMobileBsample4q/1]).

% A very ad-hoc arena size which should probably never have any effect.
% NOTE: This is a kludge!
-define(ARENA_DEFT, { 2, 2, 1197, 743 }).

%==============================================================================
% Mobile node processes.
%==============================================================================
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Compute the new location after bouncing off the walls of the arena.
% This does not do all possible reflections, but it's good enough here.
% Assumes at most one X-reflection and one Y-reflection.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
moveBounce(X, Y, U, V, {ArenaPosX, ArenaPosY, ArenaPosXmax, ArenaPosYmax}) ->
    % Move the node.
    X0 = X + U, Y0 = Y + V,

    % Do the X-reflections. (Try to keep sub-expressions in range.)
    { X1, Y1, U1, V1 } = if
        X0 < ArenaPosX ->
            { ArenaPosX + (ArenaPosX - X0), Y0, -U, V };
        X0 > ArenaPosXmax ->
            { ArenaPosXmax - (X0 - ArenaPosXmax), Y0, -U, V };
        true ->
            { X0, Y0, U, V }
        end,

    % Do the Y-reflections.
    if
        Y1 < ArenaPosY ->
            { X1, ArenaPosY + (ArenaPosY - Y1), U1, -V1 };
        Y1 > ArenaPosYmax ->
            { X1, ArenaPosYmax - (Y1 - ArenaPosYmax), U1, -V1 };
        true ->
            { X1, Y1, U1, V1 }
    end.
% End of moveBounce/5.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% A mobile device client process.
% To call/cast a wx_widget process on a different distributed node, may
% have to call rpc:call like this:
% Pid = rpc:call(serverD@hostA, erlang, whereis, [pidMobSimWindowB]).
% Then node(Pid) should return serverD@hostA.
% See: http://erlang.org/doc/man/wx_object.html#cast-2
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% procMobSimB(NodeName, Ntimes, Tsleep, {X, Y, U, V}, VarsMob)
%         when is_integer(Ntimes)
%         andalso is_number(Tsleep) andalso Tsleep >= 0
%         andalso is_map(VarsMob) ->
procMobSimB(PIDwx, Ntimes, Tsleep, {X, Y, U, V}, VarsMob)
        when is_pid(PIDwx) andalso is_integer(Ntimes)
        andalso is_number(Tsleep) andalso Tsleep >= 0
        andalso is_map(VarsMob) ->
    TraceMsg = maps:get(traceMsg, VarsMob, true),

    % If there is no info about the current arena size, ask for it.
    FoundArenaSize = maps:is_key(arena, VarsMob),
    TwaitArenaSize = if not FoundArenaSize ->
%        { pidMobSimWindowB, NodeName } ! {self(), req_arena_size},
        % http://erlang.org/doc/man/wx_object.html#cast-2
        wx_object:cast(PIDwx, {self(), req_arena_size}),
        10;
    true -> 0
    end,

    % First check to see if there are any relevant updates.
    VarsMobNew = receive
    { PIDserverRXa, arenaUpdate, NewArena } ->
        if TraceMsg ->
            io:format("procMobSimB ~p received arenaUpdate ~p from ~p~n",
                [self(), NewArena, PIDserverRXa]);
        true -> ok
        end,
        % Should check that the size update is from the correct server.
        % However, (probably) it will be the same server that we send to.
        maps:put(arena, NewArena, VarsMob)
    after TwaitArenaSize -> VarsMob  % Usually an immediate time-out.
    end,

    % Find out the current arena size.
    Arena = maps:get(arena, VarsMobNew, ?ARENA_DEFT),

    % Move the mobile node to its new location.
    { Xnew, Ynew, Unew, Vnew } = moveBounce(X, Y, U, V, Arena),
    if
        Ntimes > 0 ->
            Msg = pos;
        true ->
            Msg = fin
        end,

    % Send a message.
    if TraceMsg ->
        io:format("procMobSimB ~p sending ~p ~p to ~p: ~p~n",
            [self(), Msg, Ntimes, PIDwx, {X, Y, Xnew, Ynew}]);
    true -> ok
    end,

    % http://erlang.org/doc/reference_manual/expressions.html#send
    % pidMobSimWindowB must be a registered name.
    %   Example: PID of spawned server process.
    % NodeName must be a "node name". Example: "serverD@hostA".
    %   This is the -sname parameter for the erl Unix-process.
%    { pidMobSimWindowB, NodeName } !
%        {self(), Msg, Ntimes, {X, Y, Xnew, Ynew}},
    wx_object:cast(PIDwx, {self(), Msg, Ntimes, {X, Y, Xnew, Ynew}}),

    % Wait for a response which might not arrive.
    Tout = 1000,
    if TraceMsg ->
        io:format("procMobSimB ~p waiting for response [~p]~n",
            [self(), Ntimes]);
    true -> ok
    end,
    receive
        { PIDserverRX, pos_resp, NtimesRX } ->
            if TraceMsg ->
                io:format("procMobSimB ~p received pos_resp ~p from ~p~n",
                    [self(), NtimesRX, PIDserverRX]);
            true -> ok
            end;
        { PIDserverRX, fin_resp, NtimesRX } ->
            if TraceMsg ->
                io:format("procMobSimB ~p received fin_resp ~p from ~p~n",
                    [self(), NtimesRX, PIDserverRX]);
            true -> ok
            end;
        Evt ->
            if TraceMsg ->
                io:format("procMobSimB ~p received unknown message ~p~n",
                    [self(), Evt]);
            true -> ok
            end
    after
        % Time-out handler.
        Tout ->
            if TraceMsg ->
                io:format("procMobSimB ~p time-out after ~p mS~n",
                    [self(), Tout]);
            true -> ok
            end
    end,

    % The end-game.
    if Ntimes =< 0 ->
        if TraceMsg ->
            io:format("procMobSimB ~p END~n", [self()]);
        true -> ok
        end;
    true ->
        % Wait for a while before sending next message.
        if TraceMsg ->
            io:format("procMobSimB ~p sleep ~p~n", [self(), Tsleep]);
        true -> ok
        end,
        timer:sleep(Tsleep),
        % Loop and do it all again.
%        procMobSimB(NodeName, Ntimes - 1, Tsleep, {Xnew, Ynew, Unew, Vnew},
%            VarsMobNew)
        procMobSimB(PIDwx, Ntimes - 1, Tsleep, {Xnew, Ynew, Unew, Vnew},
            VarsMobNew)
    end.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Start a mobile devices, which is a client for the window process.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startMobileB(PIDwx, Ntimes, Tsleep, {X, Y, U, V}, VarsMob)
        when is_map(VarsMob) ->
    % http://erlang.org/doc/man/erlang.html#spawn-3
    spawn(mobsim4d,
        procMobSimB, [PIDwx, Ntimes, Tsleep, {X, Y, U, V}, VarsMob]).
% Noisy version.
startMobileB(PIDwx, Ntimes, Tsleep, {X, Y, U, V}) ->
    startMobileB(PIDwx, Ntimes, Tsleep, {X, Y, U, V}, #{}).
% Quiet version.
startMobileBq(PIDwx, Ntimes, Tsleep, {X, Y, U, V}) ->
    startMobileB(PIDwx, Ntimes, Tsleep, {X, Y, U, V}, #{traceMsg => false}).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% A single mobile device process, just for amusement.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startMobileB(PIDwx) ->
    startMobileB(PIDwx, 10, 2000, {10, 20, 30, 40}).
startMobileBq(PIDwx) ->
    startMobileBq(PIDwx, 10, 2000, {10, 20, 30, 40}).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Some mobile device processes, just for amusement.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startMobileBsample1(PIDwx, VarsMob) when is_map(VarsMob) ->
    startMobileB(PIDwx, 8, 1000, {550, 50, -10, 30}, VarsMob),
    startMobileB(PIDwx, 6, 1500, {600, 50, -10, 30}, VarsMob),
    startMobileB(PIDwx, 4, 2000, {100, 350, 30, -40}, VarsMob),
    startMobileB(PIDwx, 5, 2500, {150, 50, 40, 30}, VarsMob),
    startMobileB(PIDwx, 8, 1250, {450, 50, -10, 30}, VarsMob),
    startMobileB(PIDwx, 3, 3500, {100, 250, 30, -40}, VarsMob),
    startMobileB(PIDwx, 5, 3000, {400, 200, -30, 40}, VarsMob),

    startMobileB(PIDwx, 8, 1150, {750, 50, -10, 30}, VarsMob),
    startMobileB(PIDwx, 6, 1600, {600, 550, -10, 30}, VarsMob),
    startMobileB(PIDwx, 4, 2300, {600, 350, 30, -40}, VarsMob),
    startMobileB(PIDwx, 5, 2800, {150, 650, 40, 30}, VarsMob),
    startMobileB(PIDwx, 8, 1450, {950, 250, -10, 30}, VarsMob),
    startMobileB(PIDwx, 6, 3650, {800, 650, 30, -40}, VarsMob),
    startMobileB(PIDwx, 5, 3350, {1100, 450, -30, 40}, VarsMob).
startMobileBsample1(PIDwx) ->
    startMobileBsample1(PIDwx, #{}).
startMobileBsample1q(PIDwx) ->
    startMobileBsample1(PIDwx, #{ traceMsg => false }).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Some mobile device processes: startMobileBsample1 times 4.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startMobileBsample2(PIDwx, VarsMob) when is_map(VarsMob) ->
    Tgap = 4000,
    startMobileBsample1(PIDwx, VarsMob),
    timer:sleep(Tgap),
    startMobileBsample1(PIDwx, VarsMob),
    timer:sleep(Tgap),
    startMobileBsample1(PIDwx, VarsMob),
    timer:sleep(Tgap),
    startMobileBsample1(PIDwx, VarsMob).
startMobileBsample2(PIDwx) ->
    startMobileBsample2(PIDwx, #{}).
startMobileBsample2q(PIDwx) ->
    startMobileBsample2(PIDwx, #{ traceMsg => false }).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Some mobile device processes, just for amusement.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startMobileBsample3(PIDwx, VarsMob) when is_map(VarsMob) ->
    startMobileB(PIDwx, 18, 1000, {550, 50, -10, 30}, VarsMob),
    startMobileB(PIDwx, 16, 1500, {600, 50, -10, 30}, VarsMob),
    startMobileB(PIDwx, 14, 2000, {100, 350, 30, -40}, VarsMob),
    startMobileB(PIDwx, 15, 2500, {150, 50, 40, 30}, VarsMob),
    startMobileB(PIDwx, 18, 1250, {450, 50, -10, 30}, VarsMob),
    startMobileB(PIDwx, 13, 3500, {400, 250, 30, -40}, VarsMob),
    startMobileB(PIDwx, 15, 3000, {150, 200, -30, 40}, VarsMob),

    startMobileB(PIDwx, 18, 1150, {750, 50, -10, 30}, VarsMob),
    startMobileB(PIDwx, 16, 1600, {600, 550, -10, 30}, VarsMob),
    startMobileB(PIDwx, 14, 2300, {600, 350, 30, -40}, VarsMob),
    startMobileB(PIDwx, 16, 2800, {150, 650, 40, 30}, VarsMob),
    startMobileB(PIDwx, 18, 1450, {950, 250, -10, 30}, VarsMob),
    startMobileB(PIDwx, 17, 3650, {800, 650, 30, -40}, VarsMob),
    startMobileB(PIDwx, 15, 3350, {1100, 450, -30, 40}, VarsMob).
startMobileBsample3(PIDwx) ->
    startMobileBsample3(PIDwx, #{}).
startMobileBsample3q(PIDwx) ->
    startMobileBsample3(PIDwx, #{ traceMsg => false }).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Some mobile device processes: startMobileBsample3 times 4.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startMobileBsample4(PIDwx, VarsMob) when is_map(VarsMob) ->
    Tgap = 5500,
    startMobileBsample3(PIDwx, VarsMob),
    timer:sleep(Tgap),
    startMobileBsample3(PIDwx, VarsMob),
    timer:sleep(Tgap),
    startMobileBsample3(PIDwx, VarsMob),
    timer:sleep(Tgap),
    startMobileBsample3(PIDwx, VarsMob).
startMobileBsample4(PIDwx) ->
    startMobileBsample4(PIDwx, #{}).
startMobileBsample4q(PIDwx) ->
    startMobileBsample4(PIDwx, #{ traceMsg => false }).
