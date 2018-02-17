% src/erlang/wx1.erl   2018-2-17   Alan U. Kennington.
% $Id$
% Familiarization with wxErlang library.
% Reference manual: http://erlang.org/doc/apps/wx/index.html
% See also file: /usr/local/lib/erlang/lib/wx-1.8.3/examples/simple/hello.erl

-module(wx1).

% This includes a large amount of code.
% File: /usr/local/lib/erlang/lib/wx-1.8.3/src/wx.erl
-include_lib("wx/include/wx.hrl").

-export([startWindowA/0]).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% On my system, this function has problems with Glib invoking SCIM.
% The following garbage appears after calls to:
% wxFrame:new/4. wxFrame:createStatusBar/2
%
% (clientD@puma)286> wx1:startWindowA().
% (Erlang:8988): GLib-GObject-WARNING **: plugin
%  '/usr/lib64/gtk-2.0/immodules/im-scim.so' failed to register type
%  'GtkIMContextSCIM'
% (Erlang:8988): Gtk-WARNING **: Loading IM context type 'scim' failed
% true
%
% However, a window does in fact appear!!!
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
createWindowA(ServerA) ->
    % Create a frame on the X server.
    % This call triggers the GLib-GObject-WARNING for plug-in GtkIMContextSCIM.
    io:format("Calling wxFrame:new/4.~n", []),
    FrameA = wxFrame:new(ServerA, -1, "WxErlang test A", [{size, {600, 400}}]),
    % Carriage return. Get the shell text cursor back to the left of the line.
    io:format("~n", []),

    % Create status bar.
    % This call triggers the GLib-GObject-WARNING for plug-in GtkIMContextSCIM.
    io:format("Calling wxFrame:createStatusBar/2.~n", []),
    wxFrame:createStatusBar(FrameA, []),
    % Carriage return. Get the shell text cursor back to the left of the line.
    io:format("~n", []),

    % Create tool bar.
    % This doesn't create a tool bar, and doesn't cause a Glib/SCIM warning.
    io:format("Calling wxFrame:createToolBar/2.~n", []),
    wxFrame:createToolBar(FrameA, []),
    % Carriage return. Get the shell text cursor back to the left of the line.
%    io:format("~n", []),

    % Get keyboard focus.
    % This seems to have no effect.
%    wxFrame:setFocus(FrameA),
    wxWindow:setFocus(FrameA),

    % See http://erlang.org/doc/man/wxFrame.html
    % which says that wxFrame is derived from:
    % wxTopLevelWindow, wxWindow, wxEvtHandler
    % http://erlang.org/doc/man/wxEvtHandler.html#connect-2
    % wxCloseEventType() = close_window | end_session | query_end_session
    % Events defined in file: /usr/local/lib/erlang/lib/wx-1.8.3/include/wx.hrl
    % wxClose
    wxFrame:connect(FrameA, close_window),
    wxFrame:connect(FrameA, end_session),
    wxFrame:connect(FrameA, query_end_session),

    % Other event classes and types.
    % wxChildFocus
    wxFrame:connect(FrameA, child_focus),

    % wxFocus
    wxFrame:connect(FrameA, set_focus),
    wxFrame:connect(FrameA, kill_focus),

    % wxIconize
    wxFrame:connect(FrameA, iconize),

    % wxKey
    wxFrame:connect(FrameA, 'char'),
    wxFrame:connect(FrameA, key_down),

    % wxMouse
    wxFrame:connect(FrameA, motion),
    wxFrame:connect(FrameA, left_down),
    wxFrame:connect(FrameA, left_up),
    wxFrame:connect(FrameA, middle_down),
    wxFrame:connect(FrameA, middle_up),
    wxFrame:connect(FrameA, right_down),
    wxFrame:connect(FrameA, right_up),

    % wxMove
    wxFrame:connect(FrameA, move),

    % wxSize
    wxFrame:connect(FrameA, size),

    % wxShow
    wxFrame:connect(FrameA, show),

    % wxWindowCreate
    wxFrame:connect(FrameA, create),

    % wxWindowDestroy
    wxFrame:connect(FrameA, destroy),

    % The text for wxFrame:setStatusText/3 appears at the bottom of the window.
    % See http://erlang.org/doc/man/wxFrame.html#setStatusText-2
    ok = wxFrame:setStatusText(FrameA, "Status text", []),

    % Return the newly created frame.
    FrameA.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Event-handling loop for FrameA window.
% Apparently tail-recursive.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handleWindowA(FrameA) ->
    % Note: Should use "if" or "case" statements for the following,
    % where all events has the same handling for "id" and "obj" etc.
    receive
        % Try to catch all events in a single case.
        #wx{id=Id, obj=Obj, event=EvtA} ->
%            io:format("Process ~p event id=~p, obj=~p, event=~n ~p~n",
%                [self(), Id, Obj, EvtA]),

            % Each case must return carry_on or exit_normal.
            CarryOn = case EvtA of
            #wxClose{type=TypeA} ->
                case TypeA of
                % This is the event which is called when the window is closed.
                close_window ->
                    io:format("Process ~p closing window id=~p, obj=~p~n",
                        [self(), Id, Obj]),
                    wxFrame:setStatusText(FrameA, "Closing soon...", []),
                    timer:sleep(2000),
                    wxWindow:destroy(FrameA),
%                    exit(normal);
                    exit_normal;
                % This event is never received!?
                end_session ->
                    io:format("Process ~p ending session id=~p, obj=~p~n",
                        [self(), Id, Obj]),
                    wxFrame:setStatusText(FrameA, "Ending session soon...", []),
                    timer:sleep(2000),
                    wxWindow:destroy(FrameA),
%                    exit(normal);
                    exit_normal;
                _Else ->
                    carry_on
                end;

            #wxIconize{type=TypeA, iconized=Ic} ->
                case TypeA of
                iconize ->
                    io:format("window iconize: iconized=~p~n",
                        [Ic]);
                _Else ->
                    true
                end,
                carry_on;

            #wxKey{type=TypeA, keyCode=KeyC} ->
                case TypeA of
                char ->
                    io:format("char: keyCode=~p~n",
                        [KeyC]);
                key_down ->
                    io:format("key down: keyCode=~p~n",
                        [KeyC]);
                _Else ->
                    true
                end,
                carry_on;

            #wxMouse{type=TypeA, x=X, y=Y} ->
                case TypeA of
                motion ->
                    io:format("mouse motion: "
                        "X=~p, Y=~p~n",
                        [X, Y]);
                left_down ->
                    io:format("mouse left down: "
                        "X=~p, Y=~p~n",
                        [X, Y]);
                left_up ->
                    io:format("mouse left up: "
                        "X=~p, Y=~p~n",
                        [X, Y]);
                _Else ->
                    io:format("mouse event: type=~p, "
                        "X=~p, Y=~p~n",
                        [TypeA, X, Y])
                end,
                carry_on;

            % The returned rectangle is always {0, 0, 0, 0}. Not good!?
            #wxMove{type=TypeA, pos={Ws, Hs}, rect={Xr, Yr, Wr, Hr}} ->
                case TypeA of
                move ->
                    io:format("window move: "
                        "pos=[~p, ~p], rect: X=~p, Y=~p, W=~p, H=~p~n",
                        [Ws, Hs, Xr, Yr, Wr, Hr]);
                _Else ->
                    io:format("window move: unknown type = ~p: "
                        "pos=[~p, ~p], rect: X=~p, Y=~p, W=~p, H=~p~n",
                        [TypeA, Ws, Hs, Xr, Yr, Wr, Hr])
                end,
                carry_on;

            % The returned rectangle is always {0, 0, 0, 0}. Not good!?
            #wxSize{type=TypeA, size={Ws, Hs}, rect={Xr, Yr, Wr, Hr}} ->
                case TypeA of
                size ->
                    io:format("window size: "
                        "size=[~p, ~p], rect: X=~p, Y=~p, W=~p, H=~p~n",
                        [Ws, Hs, Xr, Yr, Wr, Hr]);
                _Else ->
                    io:format("window size: unknown type = ~p: "
                        "size=[~p, ~p], rect: X=~p, Y=~p, W=~p, H=~p~n",
                        [TypeA, Ws, Hs, Xr, Yr, Wr, Hr])
                end,
                carry_on;

            #wxShow{type=TypeA} ->
                case TypeA of
                show ->
                    io:format("window show~n");
                _Else ->
                    io:format("window show: unknown type=~p ", [TypeA])
                end,
                carry_on;

            #wxWindowCreate{type=TypeA} ->
                case TypeA of
                create ->
                    io:format("window create~n");
                _Else ->
                    io:format("window create: unknown type=~p ", [TypeA])
                end,
                carry_on;

            #wxWindowDestroy{type=TypeA} ->
                case TypeA of
                destroy ->
                    io:format("window destroy~n");
                _Else ->
                    io:format("window destroy: unknown type=~p ", [TypeA])
                end,
                carry_on;

            % No matching event class.
            _Else ->
                carry_on
            end,

            % Use return value of previous "case" to decide whether to carry on.
            % This will hopefully permit tail recursion, while also
            % returning to the caller is the inner "case" terminates.
            if
                CarryOn == carry_on ->
                    handleWindowA(FrameA);
                true ->
                    ok
            end;

        % All other event classes which are "connected".
        Evt ->
            io:format("Process ~p received event ~p~n", [self(), Evt]),
            handleWindowA(FrameA)
    end.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Entry point for this module.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startWindowA() ->
    % Create a server object.
    % See http://erlang.org/doc/man/wx.html#new-0
    ServerA = wx:new(),

    % Get some info on the object.
    % See http://erlang.org/doc/man/wx.html#get_env-0
    EnvA = wx:get_env(),
    io:format("wx env = ~p~n", [EnvA]),

    % See http://erlang.org/doc/man/erlang.html#ports-0
    Plist = erlang:ports(),
    io:format("Port list = ~p~n", [Plist]),

    % See http://erlang.org/doc/man/wx.html#batch-1
    % "Batches all wx commands used in the fun. Improves performance of the
    % command processing by grabbing the wxWidgets thread so that no event
    % processing will be done before the complete batch of commands is invoked."
    % I don't understand that yet.
    % See also http://erlang.org/doc/reference_manual/expressions.html#funs
    % See also http://erlang.org/doc/reference_manual/data_types.html#id66389
    % wx:batch/2 apparently returns the return value from the batched function.
    FrameA = wx:batch(fun() -> createWindowA(ServerA) end),

    % Show the frame.
    io:format("Show frame~n", []),
    wxWindow:show(FrameA),

    % Go into a loop.
    io:format("Start event handler~n", []),
    handleWindowA(FrameA),

    % Wait for a while.
%    timer:sleep(5000),

    % Destroy everything.
    io:format("Destroy server~n", []),
    wx:destroy(),
    ok.
