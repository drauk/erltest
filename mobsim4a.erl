% src/erlang/mobsim4a.erl   2018-3-14   Alan U. Kennington.
% This module simulates a toy mobile network using wxErlang and wx_object.
% Module A is the UI module.
% Module B is the server module.
% Module C is the window server functions module.
% Module D is the mobile network functions module.

-module(mobsim4a).

-export([start/0, start/1, start_link/0, start_link/1]).

-define(SERVER_REG_NAME, mobsim4reg).   % The daemon process registration name.
-define(SERVICE_MODULE, mobsim4b).      % Provides services to the daemon.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Test for mobsim4.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Test on machine running "erl -sname serverD":
%
% (serverD@hostA)1> mobsim4a:start_link().
% ....
%
% On second machine, run for example "erl -sname clientD".
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% First find out the Process ID of the process on serverD@hostA.
%
% (clientD@hostA)3> rpc:call(serverD@puma, erlang, whereis, [mobsim4reg]).
% <7864.74.0>
%
% Alternatively run ping, and search the ni() output for mobsim4reg.
% (clientD@hostA)1> net_adm:ping(serverD@hostA).
% pong
% (clientD@hostA)2> ni().
% ....
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Remember to replace "hostA" with the real host name in rpc:call/4, etc.
% It is easiest if hostA and hostB are the same host.
% If they are different hosts, you must open (almost) all of your TCP ports.
% If they are the same host, run the two processes in different shell-windows.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Then start the mobile nodes simulation.
%
% (clientD@puma)3> mobsim4d:startMobileB(<7864.74.0>).
% <0.75.0>
% ....
% These are the currently available ready-made simulations.
%
% mobsim4d:startMobileB(<7864.74.0>).
% mobsim4d:startMobileBq(<7864.74.0>).
% mobsim4d:startMobileBsample1(<7864.74.0>).
% mobsim4d:startMobileBsample1q(<7864.74.0>).
% mobsim4d:startMobileBsample2(<7864.74.0>).
% mobsim4d:startMobileBsample2q(<7864.74.0>).
% mobsim4d:startMobileBsample3(<7864.74.0>).
% mobsim4d:startMobileBsample3q(<7864.74.0>).
% mobsim4d:startMobileBsample4(<7864.74.0>).
% mobsim4d:startMobileBsample4q(<7864.74.0>).
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%==============================================================================
% With the wx_object (or gen_server) approach to event handling, it is not
% possible to send asynchronous messages to the window server.
% The closest approximation to this is the "cast" function.
% Then the handle_cast function must reply with "!" responses.
% An alternative could be to use "call" and handle_call, which is synchronous.
% But then the network simulation would be closely tied to the window server.
% The "cast" function does in fact get translated to "!" anyway.
% This can be seen in gen_server.erl, where "cast" is translated to erlang:send.
% In wx_object:cast, this is even more explicit.
%
% %% @doc Make a cast to a wx_object server.
% %% Invokes handle_cast(Request, State) in the server
% -spec cast(Obj, Request) -> ok when
%       Obj::wx:wx_object()|atom()|pid(),
%       Request::term().
% cast(#wx_ref{state=Pid}, Request) when is_pid(Pid) ->
%     Pid ! {'$gen_cast',Request},
%     ok;
% cast(Name, Request) when is_atom(Name) orelse is_pid(Name) ->
%     Name ! {'$gen_cast',Request},
%     ok.
%
% dispatch({'$gen_cast', Msg}, Mod, State) ->
%     Mod:handle_cast(Msg, State);
% dispatch(Msg = #wx{}, Mod, State) ->
%     Mod:handle_event(Msg, State);
% dispatch(Info, Mod, State) ->
%     Mod:handle_info(Info, State).
%
% So it is the WX-daemon which receives the message from the WX-module.
% Then the WX-daemon directly calls Mod:handle_cast/2.
% Thus the messages from mobile nodes could be queued in the WX-module's loop.

%==============================================================================
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% UI function to start a window server.
% http://erlang.org/doc/man/wx_object.html#start-4
% http://erlang.org/doc/man/wx_object.html#start_link-3
%
% These are presumably modelled on the gen_server start and start_link fns.
% http://erlang.org/doc/man/gen_server.html#start-3
% http://erlang.org/doc/man/gen_server.html#start_link-3
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start(ServerRegName) ->
    ServerName = {local, ServerRegName},
    Module = ?SERVICE_MODULE,
    Args = [],
    Options = [],
    wx_object:start(ServerName, Module, Args, Options).
start() ->
    start(?SERVER_REG_NAME).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This function is called from the Erlang shell or by a supervisor process.
% So probably the name of the function doesn't matter.
% wx_object:start_link/4 registers the service module as ?SERVER_REG_NAME.
% Use start_link for supervised processes. Use start for standalone processes.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start_link(ServerRegName) ->
    ServerName = {local, ServerRegName},
    Module = ?SERVICE_MODULE,
    Args = [],
    Options = [],
    wx_object:start_link(ServerName, Module, Args, Options).
start_link() ->
    start_link(?SERVER_REG_NAME).
