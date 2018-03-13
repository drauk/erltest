% src/erlang/mobsim4b.erl   2018-3-14   Alan U. Kennington.
% This module simulates a toy mobile network using wxErlang and wx_object.
% Module A is the UI module.
% Module B is the server module.
% Module C is the window server functions module.
% Module D is the mobile network functions module.

-module(mobsim4b).

% For some clues on wx_object coding, see the file:
% /usr/local/lib/erlang/lib/wx-1.8.3/examples/simple/hello2.erl
% Also: http://erlang.org/doc/man/wx_object.html
-behaviour(wx_object).

-export([init/1, handle_event/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This initial entry point must return:
% {wxObject, State} | {wxObject, State, Timeout} | ignore | {stop, Reason}
% http://erlang.org/doc/man/wx_object.html
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
init(_Args) ->
    % Initialize the window and state variables.
    { FrameB, DCclient, Dmap, Vars } = mobsim4c:initWindowB(),

    % Return the wx object FrameB, and a tuple of state variables.
    { FrameB, { FrameB, DCclient, Dmap, Vars } }.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Handle the given event in the current state.
% Return:
% {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handle_event(Evt, { FrameB, DCclient, Dmap, Vars }) ->

    % Call the event handler for this frame.
    mobsim4c:handleEventB(Evt, { FrameB, DCclient, Dmap, Vars }).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% handle_call(Msg, {From, Tag}, State) should return
%  {reply, Reply, State} | {reply, Reply, State, Timeout} | {noreply, State} |
% {noreply, State, Timeout} | {stop, Reason, Reply, State}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handle_call(Msg, { From, Tag }, State) ->
    io:format("mobsim4b:handle_call/3: Msg=~p, From=~p, Tag=~p, State=~p~n",
        [Msg, From, Tag, State]),

    % Not yet implemented.
    % ....

    Reply = reply_xyz,
    { reply, Reply, State }.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% handle_cast(Msg, State) should return
%  {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handle_cast(Msg, State) ->
%    io:format("mobsim4b:handle_cast/2: Msg=~p, State=~n~p~n", [Msg, State]),

    % Call the event handler for this message.
    mobsim4c:handleCastB(Msg, State).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Called by the supervisor of this server.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
terminate(Reason, { FrameB, DCclient, Dmap, Vars }) ->
    mobsim4c:termWindowB(Reason, { FrameB, DCclient, Dmap, Vars }).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This is, of course, just a place-holder.
% Making a hot code-change is not simple.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The arguments for wx_object and gen_server behaviours are as follows.
% The gen_server version:
% http://erlang.org/doc/man/gen_server.html#Module:code_change-3
% Module:code_change(OldVsn, State, Extra) -> {ok, NewState} | {error, Reason}
%
% The wx_object version:
% /usr/local/lib/erlang/lib/wx-1.8.3/src/wx_object.erl
% -callback code_change(OldVsn :: (term() | {'down', term()}), State :: term(),
%                       Extra :: term()) ->
%     {'ok', NewState :: term()} | {'error', Reason :: term()}.
%
% Example file:
% /local/lib/erlang/lib/wx-1.8.3/examples/simple/hello2.erl
% This seems to contradict the source and documentation.
% code_change(_, _, State) ->
%     {stop, not_yet_implemented, State}.
%
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
code_change(_OldVsn, State, _Extra) ->
    { stop, not_yet_implemented, State }.
