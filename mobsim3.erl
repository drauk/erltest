% src/erlang/mobsim3.erl   2018-2-22   Alan U. Kennington.
% This module will simulate a mobile network using wxErlang.
% Work In Progress!!!
% Added display lists and double buffering.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Test for this module.
%
% On host A, start Erlang, compile the module, and start a server.
% userA@hostA> erl -sname serverD
% (serverD@hostA)1> c(mobsim3).
% (serverD@hostA)2> mobsim3:startMobSimB().
%
% On host B, start Erlang, compile the module, and start a server.
% userB@hostB> erl -sname clientD
% (clientD@hostB)1> c(mobsim3).
% (clientD@hostB)2> mobsim3:startMobileBsample1(serverD@hostA).
%
% Remember to replace "hostA" with the real host name in startMobileBsample1().
% It is easiest if hostA and hostB are the same host.
% If they are different hosts, you must open (almost) all of your TCP ports.
% If they are the same host, run the two processes in different shell-windows.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% For a simpler client process, try this.
%
% (clientD@hostB)3> mobsim3:startMobileB(serverD@hostA).
%
% If you want to see some chaos, try this.
%
% (clientD@hostB)4> mobsim3:startMobileBsample2(serverD@hostA).
%
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-module(mobsim3).

% This includes a large amount of code.
% File: /usr/local/lib/erlang/lib/wx-1.8.3/src/wx.erl
-include_lib("wx/include/wx.hrl").

% Server process.
-export([startMobSimB/0, startWindowB/0]).

% Client process.
-export([startMobileB/1, startMobileB/4, procMobSimB/4,
    startMobileBsample1/1, startMobileBsample2/1]).

% Miscellaneous.
-export([getSname/0]).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Concatenate a list of strings.
% This should be called string:join/1, but string:* is deprecated.
% "Warning: string:join/2: deprecated; use lists:join/2 instead"
% But I don't see how lists:join/2 can accomplish this.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% See http://erlang.org/doc/man/string.html#join-2
% See http://erlang.org/doc/man/string.html#concat-2
% See http://erlang.org/doc/man/lists.html#join-2
% See http://erlang.org/doc/man/unicode.html#characters_to_list-1
% See http://erlang.org/doc/man/unicode.html#characters_to_binary-1
% Binaries, see http://erlang.org/doc/reference_manual/data_types.html#id65566
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stringListCat(L) when is_list(L) ->
    % Using separate lines makes debugging easier.
    L2 = unicode:characters_to_list(L),
    unicode:characters_to_binary(L2).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Get the "sname" flag.
% Example erl command: "erl -sname serverD".
% (serverD@hostA)1> init:get_argument(sname).
% {ok,[["serverD"]]}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% See http://erlang.org/doc/man/erl.html
% Return "" (or []) if sname is not set.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
getSname() ->
    % See http://erlang.org/doc/man/init.html#get_argument-1
    A = init:get_argument(sname),
    if
        A == error ->
            "";
        true ->
            % Focus on the list of values. Could be more than one.
            {_, L} = A,
            [[Sname]] = lists:sublist(L, 1, 1),
            Sname
    end.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% On my system, this function has problems with Glib invoking SCIM.
% The following garbage appears after calls to:
% wxFrame:new/4. wxFrame:createStatusBar/2
%
% (clientD@hostA)286> wx1:startWindowB().
% (Erlang:8988): GLib-GObject-WARNING **: plugin
%  '/usr/lib64/gtk-2.0/immodules/im-scim.so' failed to register type
%  'GtkIMContextSCIM'
% (Erlang:8988): Gtk-WARNING **: Loading IM context type 'scim' failed
% true
%
% However, a window does in fact appear!!!
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
createFrameB(ServerB) ->
    % Get the "sname" flag.
    Sname = getSname(),
    TitleText = if
        % See http://erlang.org/doc/reference_manual/expressions.html#id81948
        Sname /= "" ->
            stringListCat(["Mobile Simulation B: ", Sname]);
        true ->
            "Mobile simulation B"
        end,

    % Create a frame on the X server.
    % This call triggers the GLib-GObject-WARNING for plug-in GtkIMContextSCIM.
    io:format("Calling wxFrame:new/4.~n", []),
    % http://erlang.org/doc/man/wxFrame.html
    % http://docs.wxwidgets.org/2.8.12/wx_wxframe.html#wxframewxframe
    FrameB = wxFrame:new(ServerB, -1, TitleText,
        [{pos, {0, 0}}, {size, {1200, 800}}]),
    % Carriage return. Get the shell text cursor back to the left of the line.
    io:format("~n", []),

    % Create status bar.
    % This call triggers the GLib-GObject-WARNING for plug-in GtkIMContextSCIM.
    io:format("Calling wxFrame:createStatusBar/2.~n", []),
    wxFrame:createStatusBar(FrameB, []),
    % Carriage return. Get the shell text cursor back to the left of the line.
    io:format("~n", []),

    % Create tool bar.
    % This doesn't create a tool bar, and doesn't cause a Glib/SCIM warning.
    io:format("Calling wxFrame:createToolBar/2.~n", []),
    wxFrame:createToolBar(FrameB, []),
    % Carriage return. Get the shell text cursor back to the left of the line.

    % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    % Create MenuBar 1.
    % http://erlang.org/doc/man/wxMenuBar.html
    io:format("Calling wxMenuBar:new/0.~n", []),
    MenuBar1 = wxMenuBar:new(),

    % - - - - - - - - - - - - - - - - - -
    % Create Menu 1.
    % http://erlang.org/doc/man/wxMenu.html
    io:format("Calling wxMenu:new/1.~n", []),
    Menu1 = wxMenu:new([]),

    % Add Menu 1 to the MenuBar.
    io:format("Calling wxMenuBar:append/3.~n", []),
    wxMenuBar:append(MenuBar1, Menu1, "Menu 1"),

    % Create a MenuItem.
    % http://erlang.org/doc/man/wxMenuItem.html
    % This MenuItem becomes a Separator if "id" is not set!
    io:format("Calling wxMenuItem:new/1.~n", []),
    MenuItem1 = wxMenuItem:new(
%        [{parentMenu, Menu1}, {kind, ?wxITEM_NORMAL}, {text, "Menu Item 1"}]),
        [{id, ?wxID_ANY}, {kind, ?wxITEM_NORMAL}, {text, "Menu Item 1x"}]),

    % Add menu items to Menu 1.
    io:format("Calling wxMenu:append/2.~n", []),
%    MenuItem1a = wxMenu:append(Menu1, MenuItem1),
    wxMenu:append(Menu1, MenuItem1),
%    io:format("Calling wxMenu:appendSeparator/1.~n", []),
%    wxMenu:appendSeparator(Menu1),

    % This does override the wxMenuItem:new/2 setting for "text".
    io:format("Calling wxMenuItem:setText/2.~n", []),
    wxMenuItem:setText(MenuItem1, "Menu Item 1"),
    io:format("Calling wxMenuItem:enable/1.~n", []),
    wxMenuItem:enable(MenuItem1, [{enable, true}]),

    % Add Quit item to Menu 1.
    io:format("Calling wxMenu:appendSeparator/1.~n", []),
    wxMenu:appendSeparator(Menu1),
    io:format("Calling wxMenu:append/3.~n", []),
    wxMenu:append(Menu1, ?wxID_EXIT, "&Quit"),

    io:format("Calling wxMenuBar:enable/1.~n", []),
    wxMenuBar:enable(MenuBar1),

    % - - - - - - - - - - - - - - - - - -
    % Add the MenuBar to the Frame.
    io:format("Calling wxMenuBar:enable/1.~n", []),
    wxFrame:setMenuBar(FrameB, MenuBar1),

    % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    % Set keyboard focus.
    % This seems to have no effect.
%    wxFrame:setFocus(FrameB),
    io:format("Calling wxWindow:setFocus/1.~n", []),
    wxWindow:setFocus(FrameB),

    % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    % See http://erlang.org/doc/man/wxFrame.html
    % which says that wxFrame is derived from:
    % wxTopLevelWindow, wxWindow, wxEvtHandler
    % http://erlang.org/doc/man/wxEvtHandler.html#connect-2
    % wxCloseEventType() = close_window | end_session | query_end_session
    % Events defined in file: /usr/local/lib/erlang/lib/wx-1.8.3/include/wx.hrl
    % wxClose
    wxFrame:connect(FrameB, close_window),
    wxFrame:connect(FrameB, end_session),
    wxFrame:connect(FrameB, query_end_session),

    % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    % Other event classes and types.
    % wxActivate
    wxFrame:connect(FrameB, activate),
    wxFrame:connect(FrameB, activate_app),
    wxFrame:connect(FrameB, hibernate),

    % wxAuiManager [Advanced User Interface]
    % See http://docs.wxwidgets.org/2.8.12/wx_wxauioverview.html#wxauioverview
    wxFrame:connect(FrameB, aui_pane_button),
    wxFrame:connect(FrameB, aui_pane_close),
    wxFrame:connect(FrameB, aui_pane_maximize),
    wxFrame:connect(FrameB, aui_pane_restore),
    % The following line gives an error message at run time.
    % ** exception exit: {{badarg,aui_pane_activated}, ....
%    wxFrame:connect(FrameB, aui_pane_activated),
    wxFrame:connect(FrameB, aui_render),
    wxFrame:connect(FrameB, aui_find_manager),

    % wxAuiNotebook [Advanced User Interface]
    % (Not interesting enough to list here.)

    % wxCalendar
    % (Not relevant enough to list here.)

    % wxChildFocus
    wxFrame:connect(FrameB, child_focus),

    % wxClipboardText
    wxFrame:connect(FrameB, command_text_copy),
    wxFrame:connect(FrameB, command_text_cut),
    wxFrame:connect(FrameB, command_text_paste),

    % wxColourPicker
    wxFrame:connect(FrameB, command_colourpicker_changed),

    % wxCommand
    % (Too big, and not relevant enough to list here.)
    wxFrame:connect(FrameB, command_menu_selected),
    wxFrame:connect(FrameB, command_text_enter),
    wxFrame:connect(FrameB, command_set_focus),
    wxFrame:connect(FrameB, command_enter),

    % wxContextMenu
    wxFrame:connect(FrameB, context_menu),

    % wxDate
    wxFrame:connect(FrameB, date_changed),

    % wxDisplayChanged
    wxFrame:connect(FrameB, display_changed),

    % wxDropFiles
    wxFrame:connect(FrameB, drop_files),

    % wxErase
    wxFrame:connect(FrameB, erase_background),

    % wxFileDirPiacker
    % (Not relevant enough to list here.)

    % wxFocus
    wxFrame:connect(FrameB, set_focus),
    wxFrame:connect(FrameB, kill_focus),

    % wxFontPicker
    wxFrame:connect(FrameB, command_fontpicker_changed),

    % wxGrid
    % (Too many to list.)

    % wxHelp
    wxFrame:connect(FrameB, help),
    wxFrame:connect(FrameB, detailed_help),

    % wxHtmlLink
    wxFrame:connect(FrameB, command_html_link_clicked),

    % wxIconize
    wxFrame:connect(FrameB, iconize),

    % wxIdle
    wxFrame:connect(FrameB, idle),

    % wxInitDialog
    wxFrame:connect(FrameB, init_dialog),

    % wxJoystick
    wxFrame:connect(FrameB, joy_button_down),
    wxFrame:connect(FrameB, joy_button_up),
    wxFrame:connect(FrameB, joy_move),
    wxFrame:connect(FrameB, joy_zmove),

    % wxKey
    % These never arrive.
    wxFrame:connect(FrameB, 'char'),
    wxFrame:connect(FrameB, char_hook),
    wxFrame:connect(FrameB, key_down),
    wxFrame:connect(FrameB, key_up),

    % wxList
    % (Too many to list.)

    % wxMaximize
    wxFrame:connect(FrameB, maximize),

    % wxMenu
    wxFrame:connect(FrameB, menu_open),
    wxFrame:connect(FrameB, menu_close),
    wxFrame:connect(FrameB, menu_highlight),

    % wxMouse
    wxFrame:connect(FrameB, left_down),
    wxFrame:connect(FrameB, left_up),
    wxFrame:connect(FrameB, middle_down),
    wxFrame:connect(FrameB, middle_up),
    wxFrame:connect(FrameB, right_down),
    wxFrame:connect(FrameB, right_up),
%    wxFrame:connect(FrameB, motion),
    wxFrame:connect(FrameB, enter_window),
    wxFrame:connect(FrameB, leave_window),
    wxFrame:connect(FrameB, left_dclick),
    wxFrame:connect(FrameB, middle_dclick),
    wxFrame:connect(FrameB, right_dclick),
    wxFrame:connect(FrameB, mousewheel),

    % wxMove
%    wxFrame:connect(FrameB, move),

    % wxNavigationKey
    wxFrame:connect(FrameB, navigation_key),

    % wxNotebook
    wxFrame:connect(FrameB, command_notebook_page_changed),
    wxFrame:connect(FrameB, command_notebook_page_changing),

    % wxPaint
    % This means refresh the frame because of exposure of hidden regions.
    wxFrame:connect(FrameB, paint),

    % wxPaletteChanged
    wxFrame:connect(FrameB, palette_changed),

    % wxQueryNewPalette
    wxFrame:connect(FrameB, query_new_palette),

    % wxSash
    wxFrame:connect(FrameB, sash_dragged),

    % wxScroll
    wxFrame:connect(FrameB, scroll_top),
    wxFrame:connect(FrameB, scroll_bottom),
    wxFrame:connect(FrameB, scroll_lineup),
    wxFrame:connect(FrameB, scroll_linedown),
    wxFrame:connect(FrameB, scroll_pageup),
    wxFrame:connect(FrameB, scroll_pagedown),
    wxFrame:connect(FrameB, scroll_thumbtrack),
    wxFrame:connect(FrameB, scroll_thumbrelease),
    wxFrame:connect(FrameB, scroll_changed),

    % wxScrollWin
    wxFrame:connect(FrameB, scrollwin_top),
    wxFrame:connect(FrameB, scrollwin_bottom),
    wxFrame:connect(FrameB, scrollwin_lineup),
    wxFrame:connect(FrameB, scrollwin_linedown),
    wxFrame:connect(FrameB, scrollwin_pageup),
    wxFrame:connect(FrameB, scrollwin_pagedown),
    wxFrame:connect(FrameB, scrollwin_thumbtrack),
    wxFrame:connect(FrameB, scrollwin_thumbrelease),

    % wxSetCursor
    % This generates a huge number of events!
%    wxFrame:connect(FrameB, set_cursor),

    % wxShow
    wxFrame:connect(FrameB, show),

    % wxSize
    wxFrame:connect(FrameB, size),

    % wxSpin
    wxFrame:connect(FrameB, command_spinctrl_updated),
    wxFrame:connect(FrameB, spin_up),
    wxFrame:connect(FrameB, spin_down),
    wxFrame:connect(FrameB, spin),

    % wxSplitter
    % (Not relevant enough to list here.)

    % wxStyledText
    % (Too many to list. Probably not relevant.)

    % wxSysColourChanged
    wxFrame:connect(FrameB, sys_colour_changed),

    % wxTaskBarIcon
    % (Too many to list. Probably not relevant.)

    % wxTree
    % (Too many to list. Probably not relevant.)

    % wxUpdateUI
    % This generates a huge number of events!
%    wxFrame:connect(FrameB, update_ui),

    % wxWindowCreate
    wxFrame:connect(FrameB, create),

    % wxWindowDestroy
    wxFrame:connect(FrameB, destroy),
    % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    % The text for wxFrame:setStatusText/3 appears at the bottom of the window.
    % See http://erlang.org/doc/man/wxFrame.html#setStatusText-2
    ok = wxFrame:setStatusText(FrameB, "Mobile simulation status", []),

    % Return the newly created frame.
    FrameB.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Remove the angle brackets from around a PID's string.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pidNoBrackets(Pid) when is_pid(Pid) ->
    % Using separate lines makes debugging easier.
    L1 = pid_to_list(Pid),
    L2 = string:replace(L1, "<", "", all),
    string:replace(L2, ">", "", all).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Return one of the components of a PID's string.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pidComponent(Pid, I) when is_pid(Pid)
        andalso is_integer(I) andalso I >= 1 andalso I =< 3  ->
    % Using separate lines makes debugging easier.
    L1 = pidNoBrackets(Pid),
    L2 = string:split(L1, ".", all),
    lists:sublist(L2, I, 1).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Redraw the window from the current display list.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
drawWindowB(DCclient, Dmap) when is_map(Dmap) ->
    % See http://erlang.org/doc/man/wxBufferedDC.html
    % http://docs.wxwidgets.org/3.0/classwx_buffered_d_c.html
    DCbuf = wxBufferedDC:new(DCclient),

    % Create a white brush.
    BrushBG = wxBrush:new({255, 255, 255}),
    BrushCircle = wxBrush:new({128, 255, 128}),

    wxBufferedDC:setBackground(DCbuf, BrushBG),
    wxBufferedDC:clear(DCbuf),

    wxBufferedDC:setBrush(DCbuf, BrushCircle),

    % I really want maps:foreach/2 here, but it doesn't exist.
    % http://erlang.org/doc/man/maps.html#fold-3
    % http://erlang.org/doc/man/lists.html#foreach-2
    FnD = fun(P, { Xold, Yold, Xnew, Ynew, Nevt }, AccIn) ->
%        PIDstring = pid_to_list(P),
%        PIDstring = pidNoBrackets(P),
        PIDstring = pidComponent(P, 2),
        wxDC:drawLine(DCbuf, {Xold, Yold}, {Xnew, Ynew}),
        wxDC:drawCircle(DCbuf, {Xnew, Ynew}, 5),
        wxDC:drawPoint(DCbuf, {Xnew, Ynew}),
        % NOTE. Make a rough guess of best string location. Fix this later!
        wxDC:drawText(DCbuf,
            stringListCat([PIDstring, " [", integer_to_list(Nevt), "]"]),
            {Xnew + 3, Ynew + 3}),
        AccIn
        end,
    maps:fold(FnD, 0, Dmap),

    wxBrush:destroy(BrushCircle),
    wxBrush:destroy(BrushBG),

    % Destroying the BufferedDC transfers the buffer to the ClientDC.
    wxBufferedDC:destroy(DCbuf),
    ok.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Event-handling loop for FrameB window.
% Apparently tail-recursive.
% If exit(normal) is called, this function will not return.
% So to allow this function to return, the inner case-statement of
% a nested pair of case-statements returns either carry_on or exit_normal.
% At the end of the inner case-statement, its output-value is used in
% an if-statement to determine whether to recurse or just "fall off the end".
% If the outer case-statement "falls off the end", this function returns to
% the caller, which is startWindowB/0, which can then call wx:destroy/0.
% If exit(normal) is called in the inner loop, this function cannot return.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Dmap is the "display map", which lists all objects to be displayed.
% For lists, see:
% http://erlang.org/doc/getting_started/seq_prog.html#id62718
% http://erlang.org/doc/reference_manual/data_types.html#id77524
% http://erlang.org/doc/reference_manual/expressions.html#id83040
% http://erlang.org/doc/man/lists.html
% http://erlang.org/doc/man/erlang.html#is_list-1
%
% For maps, see:
% http://erlang.org/doc/getting_started/seq_prog.html#id62718
% http://erlang.org/doc/reference_manual/data_types.html#id77432
% http://erlang.org/doc/reference_manual/expressions.html#id83109
% http://erlang.org/doc/man/maps.html
% http://erlang.org/doc/man/erlang.html#is_map-1
%
% For arrays, see:
% http://erlang.org/doc/man/array.html#is_array-1
%
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% handleWindowB(FrameB, DCclient, Dlist) when is_list(Dlist) ->
handleWindowB(FrameB, DCclient, Dmap) when is_map(Dmap) ->
    receive
        % Try to catch all events in a single case.
        #wx{id=Id, obj=Obj, event=EvtB} ->
            % Trace all messages.
%            io:format("Process ~p event id=~p, obj=~p, event=~n ~p~n",
%                [self(), Id, Obj, EvtB]),

            % Each case must return carry_on or exit_normal.
            CarryOn = case EvtB of
            #wxClose{type=TypeB} ->
                case TypeB of
                % This is the event which is called when the window is closed.
                close_window ->
                    io:format("Process ~p closing window id=~p, obj=~p~n",
                        [self(), Id, Obj]),
                    wxFrame:setStatusText(FrameB, "Closing now...", []),
                    wxWindow:destroy(FrameB),
%                    exit(normal);
                    exit_normal;
                % This event is never received!?
                end_session ->
                    io:format("Process ~p ending session id=~p, obj=~p~n",
                        [self(), Id, Obj]),
                    wxFrame:setStatusText(FrameB, "Ending session now...", []),
                    wxWindow:destroy(FrameB),
%                    exit(normal);
                    exit_normal;
                _Else ->
                    carry_on
                end;

            #wxActivate{type=TypeB, active=Ac} ->
                case TypeB of
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

            #wxIconize{type=TypeB, iconized=Ic} ->
                case TypeB of
                iconize ->
                    io:format("window iconize: iconized=~p~n",
                        [Ic]);
                _Else ->
                    true
                end,
                carry_on;

            #wxKey{type=TypeB, keyCode=KeyC} ->
                case TypeB of
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

            #wxMouse{type=TypeB, x=X, y=Y} ->
                case TypeB of
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
                        [TypeB, X, Y])
                end,
                carry_on;

            % The returned rectangle is always {0, 0, 0, 0}. Not good!?
            #wxMove{type=TypeB, pos={Ws, Hs}, rect={Xr, Yr, Wr, Hr}} ->
                case TypeB of
                move ->
                    io:format("window move: "
                        "pos=[~p, ~p], rect: X=~p, Y=~p, W=~p, H=~p~n",
                        [Ws, Hs, Xr, Yr, Wr, Hr]);
                _Else ->
                    io:format("window move: unknown type = ~p: "
                        "pos=[~p, ~p], rect: X=~p, Y=~p, W=~p, H=~p~n",
                        [TypeB, Ws, Hs, Xr, Yr, Wr, Hr])
                end,
                carry_on;

            #wxPaint{type=TypeB} ->
                case TypeB of
                paint ->
                    io:format("window paint event=~p~n", [EvtB]),
                    % Create temporary context for the refresh.
                 % http://docs.wxwidgets.org/2.8.12/wx_wxpaintdc.html#wxpaintdc
                    DCpaint = wxPaintDC:new(FrameB),

                    % Draw all of the nodes in the display list.
                    drawWindowB(DCpaint, Dmap),

                    % Clean out the trash.
                    wxPaintDC:destroy(DCpaint);
                _Else ->
                    io:format("window paint event: unknown type=~p ", [TypeB])
                end,
                carry_on;

            % The returned rectangle is always {0, 0, 0, 0}. Not good!?
            #wxSize{type=TypeB, size={Ws, Hs}, rect={Xr, Yr, Wr, Hr}} ->
                case TypeB of
                size ->
                    io:format("window size: "
                        "size=[~p, ~p], rect: X=~p, Y=~p, W=~p, H=~p~n",
                        [Ws, Hs, Xr, Yr, Wr, Hr]);
                _Else ->
                    io:format("window size: unknown type = ~p: "
                        "size=[~p, ~p], rect: X=~p, Y=~p, W=~p, H=~p~n",
                        [TypeB, Ws, Hs, Xr, Yr, Wr, Hr])
                end,
                carry_on;

            #wxShow{type=TypeB} ->
                case TypeB of
                show ->
                    io:format("window show~n");
                _Else ->
                    io:format("window show: unknown type=~p ", [TypeB])
                end,
                carry_on;

            #wxWindowCreate{type=TypeB} ->
                case TypeB of
                create ->
                    io:format("window create~n");
                _Else ->
                    io:format("window create: unknown type=~p ", [TypeB])
                end,
                carry_on;

            #wxWindowDestroy{type=TypeB} ->
                case TypeB of
                destroy ->
                    io:format("window destroy~n");
                _Else ->
                    io:format("window destroy: unknown type=~p ", [TypeB])
                end,
                carry_on;

            % No matching event class.
            _Else ->
                io:format("Process ~p event id=~p, obj=~p, event=~n ~p~n",
                    [self(), Id, Obj, EvtB]),
                carry_on
            end,

            % Use return value of previous "case" to decide whether to carry on.
            % This will hopefully permit tail recursion, while also
            % returning to the caller is the inner "case" terminates.
            if
                CarryOn == carry_on ->
                    handleWindowB(FrameB, DCclient, Dmap);
                CarryOn == exit_normal ->
                    ok;
                true ->
                    ok
            end;

        % Handle position message from a mobile client.
        { PIDclient, pos, Ntimes, { Xold, Yold, Xnew, Ynew }} ->
            io:format("~p received position event ~p from ~p: "
                "(~p,~p,~p,~p)~n",
                [self(), Ntimes, PIDclient, Xold, Yold, Xnew, Ynew]),

            % Confirm receipt of the data.
            PIDclient ! { self(), pos_resp, Ntimes },

            % Update the display list.
%            DmapNew = Dmap#{ PIDclient => { Xold, Yold, Xnew, Ynew, Ntimes }},
            DmapNew = maps:put(PIDclient,
                { Xold, Yold, Xnew, Ynew, Ntimes }, Dmap),
            Nmobs = erlang:map_size(DmapNew),
            StrNmobs = integer_to_list(Nmobs),

            % Status text at the bottom of the window.
            % See http://erlang.org/doc/man/wxFrame.html#setStatusText-2
            ok = wxFrame:setStatusText(FrameB,
                stringListCat(["Status: N mobiles = ", StrNmobs]), []),

            io:format("~p new display list: ~p~n", [self(), DmapNew]),

            % Draw all of the nodes in the display list.
            drawWindowB(DCclient, DmapNew),

            handleWindowB(FrameB, DCclient, DmapNew);

        % Handle finish message from a mobile client.
        { PIDclient, fin, Ntimes, { Xold, Yold, Xnew, Ynew }} ->
            io:format("~p received finish event ~p from ~p: "
                "(~p,~p,~p,~p)~n",
                [self(), Ntimes, PIDclient, Xold, Yold, Xnew, Ynew]),

            % Confirm receipt of the data.
            PIDclient ! { self(), fin_resp, Ntimes },

            % Update the display list.
            DmapNew = maps:remove(PIDclient, Dmap),
            Nmobs = erlang:map_size(DmapNew),
            StrNmobs = integer_to_list(Nmobs),

            % Status text at the bottom of the window.
            % See http://erlang.org/doc/man/wxFrame.html#setStatusText-2
            ok = wxFrame:setStatusText(FrameB,
                stringListCat(["Status: N mobiles = ", StrNmobs]), []),

            io:format("~p new display list: ~p~n", [self(), DmapNew]),

            % Draw all of the nodes in the display list.
            drawWindowB(DCclient, DmapNew),

            handleWindowB(FrameB, DCclient, DmapNew);

        % All other event classes which are "connected".
        Evt ->
            io:format("Process ~p received event ~p~n", [self(), Evt]),
            handleWindowB(FrameB, DCclient, Dmap)
    end.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This function is intended to be spawned from startMobSimB.
% It calls the event handler for the window process.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startWindowB() ->
    % Create a server object.
    % See http://erlang.org/doc/man/wx.html#new-0
    ServerB = wx:new(),

    % Get some info on the object.
    % See http://erlang.org/doc/man/wx.html#get_env-0
    EnvB = wx:get_env(),
    io:format("wx env = ~p~n", [EnvB]),

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
    FrameB = wx:batch(fun() -> createFrameB(ServerB) end),

    % Show the frame.
    io:format("Show wx frame~n", []),
    wxWindow:show(FrameB),

    % Create a DC (Device Context).
    % http://erlang.org/doc/man/wxClientDC.html
    % http://docs.wxwidgets.org/3.0/classwx_client_d_c.html
    DCclient = wxClientDC:new(FrameB),

    BrushBG = wxBrush:new({255, 255, 255}),
    wxClientDC:setBackground(DCclient, BrushBG),
    wxClientDC:clear(DCclient),

    % Go into a loop.
    io:format("Start wx event handler~n", []),
    handleWindowB(FrameB, DCclient, #{}),

    % Destroy the Device Context.
    io:format("Destroy DC (Device Context)~n", []),
    wxClientDC:destroy(DCclient),

    % Destroy the wx server.
    % There can only be one, I think. So there's no need to specify which one.
    io:format("Destroy wx server~n", []),
    wx:destroy(),
    ok.

%==============================================================================
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% A mobile device client process.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procMobSimB(PIDserver, Ntimes, Tsleep, {X, Y, U, V})
        when is_integer(Ntimes) andalso Ntimes > 0
        andalso is_number(Tsleep) andalso Tsleep >= 0 ->
    Xnew = X + U, Ynew = Y + V,
    io:format("procMobSimB ~p sending pos ~p to ~p: ~p~n",
        [self(), Ntimes, PIDserver, {X, Y, Xnew, Ynew}]),

    % Send the message.
    { pidMobSimWindowB, PIDserver } !
        { self(), pos, Ntimes, {X, Y, Xnew, Ynew} },

    % Wait for a response which might not arrive.
    Tout = 1000,
    io:format("procMobSimB ~p waiting for response [~p]~n", [self(), Ntimes]),
    receive
        { PIDserverRX, pos_resp, NtimesRX } ->
            io:format("procMobSimB ~p received pos_resp response ~p from ~p~n",
                [self(), NtimesRX, PIDserverRX])
    after
        % Time-out handler.
        Tout ->
            io:format("procMobSimB ~p time-out after ~p mS~n",
                [self(), Tout])
    end,

    % Wait for a while before sending next message.
    io:format("procMobSimB ~p sleep ~p~n", [self(), Tsleep]),
    timer:sleep(Tsleep),
    procMobSimB(PIDserver, Ntimes - 1, Tsleep, {Xnew, Ynew, U, V});

% The last wake-up before terminating.
procMobSimB(PIDserver, Ntimes, _, {X, Y, U, V})
        when is_integer(Ntimes) andalso Ntimes =< 0
%        andalso is_number(Tsleep) andalso Tsleep >= 0 ->
        ->
    Xnew = X + U, Ynew = Y + V,
    io:format("procMobSimB ~p sending fin to server ~p [~p]~n",
        [self(), PIDserver, Ntimes]),

    % Send the message.
    { pidMobSimWindowB, PIDserver } !
        { self(), fin, Ntimes, {X, Y, Xnew, Ynew}},

    % Wait for a response which might not arrive.
    Tout = 1000,
    io:format("procMobSimB ~p waiting for response [~p]~n", [self(), Ntimes]),
    receive
        { PIDserverRX, fin_resp, NtimesRX } ->
            io:format("procMobSimB ~p received fin_resp response ~p from ~p~n",
                [self(), NtimesRX, PIDserverRX])
    after
        % Time-out handler.
        Tout ->
            io:format("procMobSimB ~p time-out after ~p mS~n",
                [self(), Tout])
    end,

%    timer:sleep(Tsleep),
    io:format("procMobSimB ~p END~n", [self()]).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Entry point for this module.
% This starts a window server for the mobile clients to send messages to.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Test on machine running "erl -sname serverD":
%
% >>> mobsim3:startMobSimB().
%
% On second machine, running for example "erl -sname clientD":
%
% >>> mobsim3:startMobileB(serverD@hostA, 4, 2000, {100, 350, 30, -40}).
% mobsim3:startMobileB(serverD@hostA, 5, 2000, {150, 50, 40, 30}).
%
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startMobSimB() ->
    io:format("mobsim3 process ~p spawning wxWindow process~n", [self()]),
    register(pidMobSimWindowB, spawn(mobsim3, startWindowB, [])).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Start a mobile devices, which is a client for the window process.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Test on machine running "erl -sname serverD":
%
% (serverD@hostA)42> mobsim3:startMobSimB().
% mobsim3 process <0.103.0> spawning wxWindow process
% ....
%
% On second machine, running for example "erl -sname clientD":
%
% mobsim3:startMobileB(serverD@hostA, 7, 2500, {100, 200}).
% procMobSimB <0.1727.0> sending pos 7 to serverD@hostA
% ....
%
% Alternatively, run this:
% mobsim3:startMobileBsample1(serverD@hostA).
% ....
%
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startMobileB(PIDserver, Ntimes, Tsleep, {X, Y, U, V}) ->
    spawn(mobsim3, procMobSimB, [PIDserver, Ntimes, Tsleep, {X, Y, U, V}]).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% A single mobile device process, just for amusement.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startMobileB(PIDserver) ->
    startMobileB(PIDserver, 10, 2000, {10, 20, 30, 40}).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Some mobile device processes, just for amusement.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startMobileBsample1(PIDserver) ->
    startMobileB(PIDserver, 8, 1000, {550, 50, -10, 30}),
    startMobileB(PIDserver, 6, 1500, {600, 50, -10, 30}),
    startMobileB(PIDserver, 4, 2000, {100, 350, 30, -40}),
    startMobileB(PIDserver, 5, 2500, {150, 50, 40, 30}),
    startMobileB(PIDserver, 8, 1250, {450, 50, -10, 30}),
    startMobileB(PIDserver, 3, 3500, {100, 250, 30, -40}),
    startMobileB(PIDserver, 5, 3000, {400, 200, -30, 40}),

    startMobileB(PIDserver, 8, 1150, {750, 50, -10, 30}),
    startMobileB(PIDserver, 6, 1600, {600, 550, -10, 30}),
    startMobileB(PIDserver, 4, 2300, {600, 350, 30, -40}),
    startMobileB(PIDserver, 5, 2800, {150, 650, 40, 30}),
    startMobileB(PIDserver, 8, 1450, {950, 250, -10, 30}),
    startMobileB(PIDserver, 6, 3650, {800, 650, 30, -40}),
    startMobileB(PIDserver, 5, 3350, {1100, 450, -30, 40}).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Some mobile device processes, just for 4 times as much amusement.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startMobileBsample2(PIDserver) ->
    Tgap = 4000,
    startMobileBsample1(PIDserver),
    timer:sleep(Tgap),
    startMobileBsample1(PIDserver),
    timer:sleep(Tgap),
    startMobileBsample1(PIDserver),
    timer:sleep(Tgap),
    startMobileBsample1(PIDserver).
