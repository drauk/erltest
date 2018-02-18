% src/erlang/mobsim1.erl   2018-2-18   Alan U. Kennington.
% $Id$
% Familiarization with wxErlang library.
% For wx: http://erlang.org/doc/apps/wx/index.html
% See also file: /usr/local/lib/erlang/lib/wx-1.8.3/examples/simple/hello.erl

-module(mobsim1).

% This includes a large amount of code.
% File: /usr/local/lib/erlang/lib/wx-1.8.3/src/wx.erl
-include_lib("wx/include/wx.hrl").

-export([startMobSimA/0, startWindowA/0]).

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
    FrameA = wxFrame:new(ServerA, -1,
        "Mobile simulation A", [{size, {600, 400}}]),
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

    % Set keyboard focus.
    % This seems to have no effect.
%    wxFrame:setFocus(FrameA),
    wxWindow:setFocus(FrameA),

    % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

    % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    % Other event classes and types.
    % wxActivate
    wxFrame:connect(FrameA, activate),
    wxFrame:connect(FrameA, activate_app),
    wxFrame:connect(FrameA, hibernate),

    % wxAuiManager [Advanced User Interface]
    % See http://docs.wxwidgets.org/2.8.12/wx_wxauioverview.html#wxauioverview
    wxFrame:connect(FrameA, aui_pane_button),
    wxFrame:connect(FrameA, aui_pane_close),
    wxFrame:connect(FrameA, aui_pane_maximize),
    wxFrame:connect(FrameA, aui_pane_restore),
    % The following line gives an error message at run time.
    % ** exception exit: {{badarg,aui_pane_activated}, ....
%    wxFrame:connect(FrameA, aui_pane_activated),
    wxFrame:connect(FrameA, aui_render),
    wxFrame:connect(FrameA, aui_find_manager),

    % wxAuiNotebook [Advanced User Interface]
    % (Not interesting enough to list here.)

    % wxCalendar
    % (Not relevant enough to list here.)

    % wxChildFocus
    wxFrame:connect(FrameA, child_focus),

    % wxClipboardText
    wxFrame:connect(FrameA, command_text_copy),
    wxFrame:connect(FrameA, command_text_cut),
    wxFrame:connect(FrameA, command_text_paste),

    % wxColourPicker
    wxFrame:connect(FrameA, command_colourpicker_changed),

    % wxCommand
    % (Too big, and not relevant enough to list here.)

    % wxContextMenu
    wxFrame:connect(FrameA, context_menu),

    % wxDate
    wxFrame:connect(FrameA, date_changed),

    % wxDisplayChanged
    wxFrame:connect(FrameA, display_changed),

    % wxDropFiles
    wxFrame:connect(FrameA, drop_files),

    % wxErase
    wxFrame:connect(FrameA, erase_background),

    % wxFileDirPiacker
    % (Not relevant enough to list here.)

    % wxFocus
    wxFrame:connect(FrameA, set_focus),
    wxFrame:connect(FrameA, kill_focus),

    % wxFontPicker
    wxFrame:connect(FrameA, command_fontpicker_changed),

    % wxGrid
    % (Too many to list.)

    % wxHelp
    wxFrame:connect(FrameA, help),
    wxFrame:connect(FrameA, detailed_help),

    % wxHtmlLink
    wxFrame:connect(FrameA, command_html_link_clicked),

    % wxIconize
    wxFrame:connect(FrameA, iconize),

    % wxIdle
    wxFrame:connect(FrameA, idle),

    % wxInitDialog
    wxFrame:connect(FrameA, init_dialog),

    % wxJoystick
    wxFrame:connect(FrameA, joy_button_down),
    wxFrame:connect(FrameA, joy_button_up),
    wxFrame:connect(FrameA, joy_move),
    wxFrame:connect(FrameA, joy_zmove),

    % wxKey
    wxFrame:connect(FrameA, 'char'),
    wxFrame:connect(FrameA, char_hook),
    wxFrame:connect(FrameA, key_down),
    wxFrame:connect(FrameA, key_up),

    % wxList
    % (Too many to list.)

    % wxMaximize
    wxFrame:connect(FrameA, maximize),

    % wxMenu
    wxFrame:connect(FrameA, menu_open),
    wxFrame:connect(FrameA, menu_close),
    wxFrame:connect(FrameA, menu_highlight),

    % wxMouse
    wxFrame:connect(FrameA, left_down),
    wxFrame:connect(FrameA, left_up),
    wxFrame:connect(FrameA, middle_down),
    wxFrame:connect(FrameA, middle_up),
    wxFrame:connect(FrameA, right_down),
    wxFrame:connect(FrameA, right_up),
%    wxFrame:connect(FrameA, motion),
    wxFrame:connect(FrameA, enter_window),
    wxFrame:connect(FrameA, leave_window),
    wxFrame:connect(FrameA, left_dclick),
    wxFrame:connect(FrameA, middle_dclick),
    wxFrame:connect(FrameA, right_dclick),
    wxFrame:connect(FrameA, mousewheel),

    % wxMove
%    wxFrame:connect(FrameA, move),

    % wxNavigationKey
    wxFrame:connect(FrameA, navigation_key),

    % wxNotebook
    wxFrame:connect(FrameA, command_notebook_page_changed),
    wxFrame:connect(FrameA, command_notebook_page_changing),

    % wxPaint
    wxFrame:connect(FrameA, paint),

    % wxPaletteChanged
    wxFrame:connect(FrameA, palette_changed),

    % wxQueryNewPalette
    wxFrame:connect(FrameA, query_new_palette),

    % wxSash
    wxFrame:connect(FrameA, sash_dragged),

    % wxScroll
    wxFrame:connect(FrameA, scroll_top),
    wxFrame:connect(FrameA, scroll_bottom),
    wxFrame:connect(FrameA, scroll_lineup),
    wxFrame:connect(FrameA, scroll_linedown),
    wxFrame:connect(FrameA, scroll_pageup),
    wxFrame:connect(FrameA, scroll_pagedown),
    wxFrame:connect(FrameA, scroll_thumbtrack),
    wxFrame:connect(FrameA, scroll_thumbrelease),
    wxFrame:connect(FrameA, scroll_changed),

    % wxScrollWin
    wxFrame:connect(FrameA, scrollwin_top),
    wxFrame:connect(FrameA, scrollwin_bottom),
    wxFrame:connect(FrameA, scrollwin_lineup),
    wxFrame:connect(FrameA, scrollwin_linedown),
    wxFrame:connect(FrameA, scrollwin_pageup),
    wxFrame:connect(FrameA, scrollwin_pagedown),
    wxFrame:connect(FrameA, scrollwin_thumbtrack),
    wxFrame:connect(FrameA, scrollwin_thumbrelease),

    % wxSetCursor
    wxFrame:connect(FrameA, set_cursor),

    % wxShow
    wxFrame:connect(FrameA, show),

    % wxSize
    wxFrame:connect(FrameA, size),

    % wxSpin
    wxFrame:connect(FrameA, command_spinctrl_updated),
    wxFrame:connect(FrameA, spin_up),
    wxFrame:connect(FrameA, spin_down),
    wxFrame:connect(FrameA, spin),

    % wxSplitter
    % (Not relevant enough to list here.)

    % wxStyledText
    % (Too many to list. Probably not relevant.)

    % wxSysColourChanged
    wxFrame:connect(FrameA, sys_colour_changed),

    % wxTaskBarIcon
    % (Too many to list. Probably not relevant.)

    % wxTree
    % (Too many to list. Probably not relevant.)

    % wxUpdateUI
    wxFrame:connect(FrameA, update_ui),

    % wxWindowCreate
    wxFrame:connect(FrameA, create),

    % wxWindowDestroy
    wxFrame:connect(FrameA, destroy),
    % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    % The text for wxFrame:setStatusText/3 appears at the bottom of the window.
    % See http://erlang.org/doc/man/wxFrame.html#setStatusText-2
    ok = wxFrame:setStatusText(FrameA, "Mobile simulation status", []),

    % Return the newly created frame.
    FrameA.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Event-handling loop for FrameA window.
% Apparently tail-recursive.
% If exit(normal) is called, this function will not return.
% So to allow this function to return, the inner case-statement of
% a nested pair of case-statements returns either carry_on or exit_normal.
% At the end of the inner case-statement, its output-value is used in
% an if-statement to determine whether to recurse or just "fall off the end".
% If the outer case-statement "falls off the end", this function returns to
% the caller, which is startWindowA/0, which can then call wx:destroy/0.
% If exit(normal) is called in the inner loop, this function cannot return.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handleWindowA(FrameA) ->
    % Note: Should use "if" or "case" statements for the following,
    % where all events has the same handling for "id" and "obj" etc.
    receive
        % Try to catch all events in a single case.
        #wx{id=Id, obj=Obj, event=EvtA} ->
            % Trace all messages.
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

            #wxActivate{type=TypeA, active=Ac} ->
                case TypeA of
                activate ->
                    io:format("window activate: active=~p~n",
                        [Ac]);
                activate_app ->
                    io:format("window activate app: active=~p~n",
                        [Ac]);
                hibernate ->
                    io:format("window hibernate: active=~p~n",
                        [Ac]);
                _Else ->
                    true
                end,
                carry_on;

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
                CarryOn == exit_normal ->
                    ok;
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
% But this function is intended to be spawned from startMobSimA.
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
    io:format("Show wx frame~n", []),
    wxWindow:show(FrameA),

    % Go into a loop.
    io:format("Start wx event handler~n", []),
    handleWindowA(FrameA),

    % Wait for a while.
%    timer:sleep(5000),

    % Destroy everything.
    io:format("Destroy wx server~n", []),
    wx:destroy(),
    ok.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Entry point for this module.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startMobSimA() ->
    io:format("mobsim1 process ~p spawning wxWindow process~n",
        [self()]),
    register(pidMobSimWindowA, spawn(mobsim1, startWindowA, [])).
