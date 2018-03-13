% src/erlang/mobsim4c.erl   2018-3-14   Alan U. Kennington.
% This module simulates a toy mobile network using wxErlang and wx_object.
% Module A is the UI module.
% Module B is the server module.
% Module C is the window server functions module.
% Module D is the mobile network functions module.

-module(mobsim4c).

% This includes a large amount of code.
% File: /usr/local/lib/erlang/lib/wx-1.8.3/src/wx.erl
-include_lib("wx/include/wx.hrl").

% Server process.
-export([initWindowB/0, termWindowB/2, handleEventB/2, handleCastB/2]).

% Miscellaneous.
-export([getSname/0]).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Default initial Frame position and size.
-define(FRAME_POS_X, 0).
-define(FRAME_POS_Y, 0).
-define(FRAME_POS_INIT, {?FRAME_POS_X, ?FRAME_POS_Y}).

-define(FRAME_SIZE_W, 1200).
-define(FRAME_SIZE_H, 800).
-define(FRAME_SIZE_INIT, {?FRAME_SIZE_W, ?FRAME_SIZE_H}).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The arena of action for the mobile nodes.
-define(ARENA_INDENT, 2).
-define(ARENA_POS_X, ?ARENA_INDENT).
-define(ARENA_POS_Y, ?ARENA_INDENT).
-define(ARENA_POS_INIT, {?ARENA_POS_X, ?ARENA_POS_Y}).

% NOTE: Must also take into account the heights of various panels.
-define(ARENA_SIZE_H_PANEL, 53).    % This is a fudge! My system uses 53.
-define(ARENA_SIZE_W, (?FRAME_SIZE_W - 1 - ?ARENA_INDENT)).
-define(ARENA_SIZE_H,
    (?FRAME_SIZE_H - 1 - ?ARENA_INDENT - ?ARENA_SIZE_H_PANEL)).
-define(ARENA_SIZE_INIT, {?ARENA_SIZE_W, ?ARENA_SIZE_H}).
-define(ARENA_POS_Xmax, (?ARENA_POS_X + ?ARENA_SIZE_W - 1)).
-define(ARENA_POS_Ymax, (?ARENA_POS_Y + ?ARENA_SIZE_H - 1)).

-define(ARENA_BDY_COLOUR, {255, 255, 255}).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Some constants for menu items.
% Must avoid range 5000 to 6000, and probably below about 120 also.
% There should be a systematic way to allocate these ID values.
% Place-holders for "future growth"!!
-define(MENU_ITEM_1, 1001).
-define(MENU_ITEM_2, 1002).
-define(MENU_ITEM_3, 1003).

% - - - - - - - - - - - - - - - - - - -
% Node colours.
-define(MENU_ITEM_R, 1010).
-define(MENU_ITEM_G, 1011).             % Default.
-define(MENU_ITEM_B, 1012).

% Some colours.
-define(NODE_COLOUR_R, {255, 128, 128}).
-define(NODE_COLOUR_G, {128, 255, 128}).
-define(NODE_COLOUR_B, {128, 128, 255}).

-define(NODE_COLOUR_DEFT, ?NODE_COLOUR_G).

% - - - - - - - - - - - - - - - - - - -
% Node shapes.
-define(MENU_ITEM_CIRCLE, 1020).
-define(MENU_ITEM_SQUARE, 1021).
-define(MENU_ITEM_PENTAGON, 1022).
-define(MENU_ITEM_HEXAGON, 1023).
-define(MENU_ITEM_X, 1024).

-define(NODE_SHAPE_DEFT, nodeShapeHexagon).

% - - - - - - - - - - - - - - - - - - -
% Node sizes.
-define(MENU_ITEM_RAD1, 1030).          % Default.
-define(MENU_ITEM_RAD2, 1031).
-define(MENU_ITEM_RAD3, 1032).
-define(MENU_ITEM_RAD4, 1033).

% Some radius options
-define(NODE_RAD1, 5).
-define(NODE_RAD2, 10).
-define(NODE_RAD3, 20).
-define(NODE_RAD4, 40).

-define(NODE_RAD_DEFT, ?NODE_RAD2).

% - - - - - - - - - - - - - - - - - - -
% Trace options.
-define(MENU_ITEM_TRACE_DMAP, 1040).    % Default is off.
-define(TRACE_DMAP_DEFT, false).

-define(MENU_ITEM_TRACE_MOUSE, 1041).   % Default is off.
-define(TRACE_MOUSE_DEFT, false).

-define(MENU_ITEM_TRACE_MOTION, 1042).  % Default is off.
-define(TRACE_MOTION_DEFT, false).

-define(MENU_ITEM_TRACE_CURSOR, 1043).  % Default is off.
-define(TRACE_CURSOR_DEFT, false).

-define(MENU_ITEM_TRACE_WINDOW, 1044).  % Default is off.
-define(TRACE_WINDOW_DEFT, false).

-define(MENU_ITEM_TRACE_MENU, 1045).    % Default is off.
-define(TRACE_MENU_DEFT, false).

-define(MENU_ITEM_TRACE_UPDATE_UI, 1046). % Default is off.
-define(TRACE_UPDATE_UI_DEFT, false).

-define(MENU_ITEM_TRACE_NODE, 1047).    % Default is on.
-define(TRACE_NODE_DEFT, true).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Convert integer 0 or 1 to boolean false or true.
% For other values, return the default.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int_to_boolean(Int, Default)
        when is_integer(Int) andalso is_boolean(Default) ->
    % Erlang should have a test like: "Value = (Int /= 0)".
    case Int of
        0 ->
            false;
        1 ->
            true;
        _Else ->
            Default
    end.
% End of int_to_boolean/2.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The default here is "false" because this function is intended for situations
% where 0 and 1 are the only expected value for Int.
% Therefore any other value is considered to be an error.
% So then it is safer to do nothing.
% This default value is the opposite of the convention in C/C++.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int_to_boolean(Int) when is_integer(Int) ->
    int_to_boolean(Int, false).
% End of int_to_boolean/1.

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
    case A of
        error ->
            "";
        {ok, L} ->
            % Focus on the list of values. Could be more than one.
            [[Sname]] = lists:sublist(L, 1, 1),
            Sname;
        true ->
            ""
     end.
% End of getSname/0.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% See http://erlang.org/doc/man/inet.html#gethostname-0
% The documentation says that inet:gethostname() will always succeed.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
getHostname() ->
    {ok, H} = inet:gethostname(),
    H.
% End of getHostname/0.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Remove the angle brackets from around a PID's string.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pidNoBrackets(Pid) when is_pid(Pid) ->
    % Using separate lines makes debugging easier.
    L1 = pid_to_list(Pid),
    L2 = string:replace(L1, "<", "", all),
    string:replace(L2, ">", "", all).
% End of pidNoBrackets/1.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Return one of the components of a PID's string.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pidComponent(Pid, I) when is_pid(Pid)
        andalso is_integer(I) andalso I >= 1 andalso I =< 3  ->
    % Using separate lines makes debugging easier.
    L1 = pidNoBrackets(Pid),
    L2 = string:split(L1, ".", all),
    lists:sublist(L2, I, 1).
% End of pidComponent/2.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Return the { Xmin, Ymin, Xmax, Ymax } arena for the given window size.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
calcArena({ Wclient, Hclient }) ->
    % Compute the new arena boundaries.
    ArenaPosX = ?ARENA_INDENT,
    ArenaPosY = ?ARENA_INDENT,
    ArenaPosXmax = Wclient - 1 - ?ARENA_INDENT,
    ArenaPosYmax = Hclient - 1 - ?ARENA_INDENT,
    { ArenaPosX, ArenaPosY, ArenaPosXmax, ArenaPosYmax }.
% End of calcArena/1.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Return the { X, Y, W, H } arena for the given window size.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
calcArenaXYWH({ Wclient, Hclient }) ->
    % Compute the new arena boundaries.
    ArenaPosX = ?ARENA_INDENT,
    ArenaPosY = ?ARENA_INDENT,
    ArenaPosW = Wclient - ?ARENA_INDENT * 2,
    ArenaPosH = Hclient - ?ARENA_INDENT * 2,

    { ArenaPosX, ArenaPosY, ArenaPosW, ArenaPosH }.
% End of calcArenaXYWH/1.

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
    Hostname = getHostname(),
    TitleText = if
        % See http://erlang.org/doc/reference_manual/expressions.html#id81948
        Sname /= "" ->
            "Mobile Simulation B: " ++ Sname ++ "@" ++ Hostname;
        true ->
            "Mobile Simulation B: " ++ Hostname
        end,

    % Create a frame on the X server.
    % This call triggers the GLib-GObject-WARNING for plug-in GtkIMContextSCIM.
%    io:format("Calling wxFrame:new/4.~n", []),
    % http://erlang.org/doc/man/wxFrame.html
    % http://docs.wxwidgets.org/2.8.12/wx_wxframe.html#wxframewxframe
    FrameB = wxFrame:new(ServerB, -1, TitleText,
        [{pos, ?FRAME_POS_INIT}, {size, ?FRAME_SIZE_INIT}]),
    % Carriage return. Get the shell text cursor back to the left of the line.
    io:format("~n", []),

    % Create status bar.
    % This call triggers the GLib-GObject-WARNING for plug-in GtkIMContextSCIM.
%    io:format("Calling wxFrame:createStatusBar/2.~n", []),
    wxFrame:createStatusBar(FrameB, []),
    % Carriage return. Get the shell text cursor back to the left of the line.
    io:format("~n", []),

    % Create tool bar.
    % This doesn't create a tool bar, and doesn't cause a Glib/SCIM warning.
%    io:format("Calling wxFrame:createToolBar/2.~n", []),
    wxFrame:createToolBar(FrameB, []),
    % Carriage return. Get the shell text cursor back to the left of the line.

    % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    % Create MenuBar.
    % http://erlang.org/doc/man/wxMenuBar.html
    MenuBar1 = wxMenuBar:new(),

    % - - - - - - - - - - - - - - - - - -
    % Create Menu 1.
    % http://erlang.org/doc/man/wxMenu.html
    Menu1 = wxMenu:new([]),

    % Add Menu to MenuBar.
    wxMenuBar:append(MenuBar1, Menu1, "Menu 1"),

    % Create some menu items.
    % http://erlang.org/doc/man/wxMenuItem.html
    % This MenuItem becomes a Separator if "id" is not set!
    MenuItem1 = wxMenuItem:new(
        [{id, ?MENU_ITEM_1}, {kind, ?wxITEM_NORMAL}, {text, "Menu Item 1"}]),
    MenuItem2 = wxMenuItem:new(
        [{id, ?MENU_ITEM_2}, {kind, ?wxITEM_NORMAL}, {text, "Menu Item 2"}]),
    MenuItem3 = wxMenuItem:new(
        [{id, ?MENU_ITEM_3}, {kind, ?wxITEM_NORMAL}, {text, "Menu Item 3"}]),

    % Add menu items to menu.
    wxMenu:append(Menu1, MenuItem1),
    wxMenu:append(Menu1, MenuItem2),
    wxMenu:append(Menu1, MenuItem3),

    % This does override the wxMenuItem:new/2 setting for "text".
    wxMenuItem:setText(MenuItem1, "Menu Item 1"),
    wxMenuItem:setText(MenuItem2, "Menu Item 2"),
    wxMenuItem:setText(MenuItem3, "Menu Item 3"),

    % Add Quit item to Menu.
    wxMenu:appendSeparator(Menu1),
    % Standard events IDs: http://docs.wxwidgets.org/2.8.12/wx_stdevtid.html
    % Avoid the range 5000 to 6000.
    wxMenu:append(Menu1, ?wxID_EXIT, "&Quit"),

    % - - - - - - - - - - - - - - - - - -
    % Create Menu 2.
    % http://erlang.org/doc/man/wxMenu.html
    Menu2 = wxMenu:new([]),
    wxMenuBar:append(MenuBar1, Menu2, "Node colour"),

    % Create menu items, indexed by their RGB values.
    % http://erlang.org/doc/man/wxMenuItem.html
    % Each MenuItem becomes a Separator if "id" is not set!
    ColourMenus = [
    { ?NODE_COLOUR_R, wxMenuItem:new(
        [{id, ?MENU_ITEM_R}, {kind, ?wxITEM_RADIO}, {text, "Red"}]) },
    { ?NODE_COLOUR_G, wxMenuItem:new(
        [{id, ?MENU_ITEM_G}, {kind, ?wxITEM_RADIO}, {text, "Green"}]) },
    { ?NODE_COLOUR_B, wxMenuItem:new(
        [{id, ?MENU_ITEM_B}, {kind, ?wxITEM_RADIO}, {text, "Blue"}]) }
    ],

    % Add menu items to the menu.
    lists:foreach(fun({_K, M}) -> wxMenu:append(Menu2, M) end, ColourMenus),

    % Check the radio button for the default colour.
    % http://erlang.org/doc/man/lists.html#keyfind-3
    % http://erlang.org/doc/reference_manual/data_types.html#id77368
    % http://erlang.org/doc/man/erlang.html#element-2
    % Very clumsy, but it does ensure menu/default synchronization.
    wxMenuItem:check(
        element(2, lists:keyfind(?NODE_COLOUR_DEFT, 1, ColourMenus)),
        [{check, true}]),

    % - - - - - - - - - - - - - - - - - -
    % Create Menu 3.
    % http://erlang.org/doc/man/wxMenu.html
    Menu3 = wxMenu:new([]),
    wxMenuBar:append(MenuBar1, Menu3, "Node shape"),

    % Create menu items, indexed by their corresponding literals.
    % http://erlang.org/doc/man/wxMenuItem.html
    ShapeMenus = [
    { nodeShapeCircle, wxMenuItem:new([{id, ?MENU_ITEM_CIRCLE},
        {kind, ?wxITEM_RADIO}, {text, "Circle"}]) },
    { nodeShapeSquare, wxMenuItem:new([{id, ?MENU_ITEM_SQUARE},
        {kind, ?wxITEM_RADIO}, {text, "Square"}]) },
    { nodeShapePentagon, wxMenuItem:new([{id, ?MENU_ITEM_PENTAGON},
        {kind, ?wxITEM_RADIO}, {text, "Pentagon"}]) },
    { nodeShapeHexagon, wxMenuItem:new([{id, ?MENU_ITEM_HEXAGON},
        {kind, ?wxITEM_RADIO}, {text, "Hexagon"}]) },
    { nodeShapeX, wxMenuItem:new([{id, ?MENU_ITEM_X},
        {kind, ?wxITEM_RADIO}, {text, "X"}]) }
    ],

    % Add items to the menu.
    lists:foreach(fun({_K, M}) -> wxMenu:append(Menu3, M) end, ShapeMenus),

    % Check the radio button for the default shape.
    wxMenuItem:check(
        element(2, lists:keyfind(?NODE_SHAPE_DEFT, 1, ShapeMenus)),
        [{check, true}]),

    % - - - - - - - - - - - - - - - - - -
    % Create Menu 4.
    % http://erlang.org/doc/man/wxMenu.html
    Menu4 = wxMenu:new([]),
    wxMenuBar:append(MenuBar1, Menu4, "Node radius"),

    % Create menu items.
    RadiusMenus = [
    { ?NODE_RAD1, wxMenuItem:new([{id, ?MENU_ITEM_RAD1},
        {kind, ?wxITEM_RADIO}, {text,
            integer_to_list(?NODE_RAD1) ++ " pixels"}]) },
    { ?NODE_RAD2, wxMenuItem:new([{id, ?MENU_ITEM_RAD2},
        {kind, ?wxITEM_RADIO}, {text,
            integer_to_list(?NODE_RAD2) ++ " pixels"}]) },
    { ?NODE_RAD3, wxMenuItem:new([{id, ?MENU_ITEM_RAD3},
        {kind, ?wxITEM_RADIO}, {text,
            integer_to_list(?NODE_RAD3) ++ " pixels"}]) },
    { ?NODE_RAD4, wxMenuItem:new([{id, ?MENU_ITEM_RAD4},
        {kind, ?wxITEM_RADIO}, {text,
            integer_to_list(?NODE_RAD4) ++ " pixels"}]) }
    ],

    % Add items to the menu.
    lists:foreach(fun({_K, M}) -> wxMenu:append(Menu4, M) end, RadiusMenus),

    % Check the radio button for the default.
    wxMenuItem:check(
        element(2, lists:keyfind(?NODE_RAD_DEFT, 1, RadiusMenus)),
        [{check, true}]),

    % - - - - - - - - - - - - - - - - - -
    % Menu 5.
    Menu5 = wxMenu:new([]),
    wxMenuBar:append(MenuBar1, Menu5, "Trace options"),

    % Menu items.
    MenuItemTraceMouse = wxMenuItem:new([{id, ?MENU_ITEM_TRACE_MOUSE},
        {kind, ?wxITEM_CHECK}, {text, "Mouse events"}]),
    MenuItemTraceMotion = wxMenuItem:new([{id, ?MENU_ITEM_TRACE_MOTION},
        {kind, ?wxITEM_CHECK}, {text, "Motion events"}]),
    MenuItemTraceCursor = wxMenuItem:new([{id, ?MENU_ITEM_TRACE_CURSOR},
        {kind, ?wxITEM_CHECK}, {text, "Cursor events"}]),
    MenuItemTraceWindow = wxMenuItem:new([{id, ?MENU_ITEM_TRACE_WINDOW},
        {kind, ?wxITEM_CHECK}, {text, "Window events"}]),
    MenuItemTraceMenu = wxMenuItem:new([{id, ?MENU_ITEM_TRACE_MENU},
        {kind, ?wxITEM_CHECK}, {text, "Menu events"}]),
    MenuItemTraceUpdateUI = wxMenuItem:new([{id, ?MENU_ITEM_TRACE_UPDATE_UI},
        {kind, ?wxITEM_CHECK}, {text, "UpdateUI events"}]),
    MenuItemTraceDmap = wxMenuItem:new([{id, ?MENU_ITEM_TRACE_DMAP},
        {kind, ?wxITEM_CHECK}, {text, "Display list"}]),
    MenuItemTraceNode = wxMenuItem:new([{id, ?MENU_ITEM_TRACE_NODE},
        {kind, ?wxITEM_CHECK}, {text, "Node events"}]),

    % Add menu items to the menu.
    wxMenu:append(Menu5, MenuItemTraceMouse),
    wxMenu:append(Menu5, MenuItemTraceMotion),
    wxMenu:append(Menu5, MenuItemTraceCursor),
    wxMenu:append(Menu5, MenuItemTraceWindow),
    wxMenu:append(Menu5, MenuItemTraceMenu),
    wxMenu:append(Menu5, MenuItemTraceUpdateUI),
    wxMenu:append(Menu5, MenuItemTraceDmap),
    wxMenu:append(Menu5, MenuItemTraceNode),

    % Show the defaults check/uncheck.
    wxMenuItem:check(MenuItemTraceMouse, [{check, ?TRACE_MOUSE_DEFT}]),
    wxMenuItem:check(MenuItemTraceMotion, [{check, ?TRACE_MOTION_DEFT}]),
    wxMenuItem:check(MenuItemTraceCursor, [{check, ?TRACE_CURSOR_DEFT}]),
    wxMenuItem:check(MenuItemTraceWindow, [{check, ?TRACE_WINDOW_DEFT}]),
    wxMenuItem:check(MenuItemTraceMenu, [{check, ?TRACE_MENU_DEFT}]),
    wxMenuItem:check(MenuItemTraceUpdateUI, [{check, ?TRACE_UPDATE_UI_DEFT}]),
    wxMenuItem:check(MenuItemTraceDmap, [{check, ?TRACE_DMAP_DEFT}]),
    wxMenuItem:check(MenuItemTraceNode, [{check, ?TRACE_NODE_DEFT}]),

    % - - - - - - - - - - - - - - - - - -
    % Add the MenuBar to the Frame.
    wxFrame:setMenuBar(FrameB, MenuBar1),

    % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    % Set keyboard focus.
    % This seems to have no effect.
%    wxFrame:setFocus(FrameB),
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

    % Callback handler for command_menu_selected.
    % This does actually get called. But I want to exit the loop, not crash out.
%    CmdMenuSelHand = fun(EventRecord, EventObject) ->
%        io:format("event record=~p~nevent object=~p~n",
%            [EventRecord, EventObject])
%        end,
%    wxFrame:connect(FrameB, command_menu_selected,
%        [{callback, CmdMenuSelHand}]),

    % wxCommand
    wxFrame:connect(FrameB, command_button_clicked),
    wxFrame:connect(FrameB, command_checkbox_clicked),
    wxFrame:connect(FrameB, command_choice_selected),
    wxFrame:connect(FrameB, command_listbox_selected),
    wxFrame:connect(FrameB, command_listbox_doubleclicked),
    wxFrame:connect(FrameB, command_text_updated),
    wxFrame:connect(FrameB, command_text_enter),
    wxFrame:connect(FrameB, command_menu_selected),
    wxFrame:connect(FrameB, command_slider_updated),
    wxFrame:connect(FrameB, command_radiobox_selected),
    wxFrame:connect(FrameB, command_radiobutton_selected),
    wxFrame:connect(FrameB, command_scrollbar_updated),
    wxFrame:connect(FrameB, command_vlbox_selected),
    wxFrame:connect(FrameB, command_combobox_selected),
    wxFrame:connect(FrameB, command_tool_rclicked),
    wxFrame:connect(FrameB, command_tool_enter),
    wxFrame:connect(FrameB, command_checklistbox_toggled),
    wxFrame:connect(FrameB, command_togglebutton_clicked),
    wxFrame:connect(FrameB, command_left_click),
    wxFrame:connect(FrameB, command_left_dclick),
    wxFrame:connect(FrameB, command_right_click),
    wxFrame:connect(FrameB, command_set_focus),
    wxFrame:connect(FrameB, command_kill_focus),
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

    % wxFileDirPicker
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
    wxFrame:connect(FrameB, motion),
    wxFrame:connect(FrameB, enter_window),
    wxFrame:connect(FrameB, leave_window),
    wxFrame:connect(FrameB, left_dclick),
    wxFrame:connect(FrameB, middle_dclick),
    wxFrame:connect(FrameB, right_dclick),
    wxFrame:connect(FrameB, mousewheel),

    % wxMove
    wxFrame:connect(FrameB, move),

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
    wxFrame:connect(FrameB, set_cursor),

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
    wxFrame:connect(FrameB, update_ui),

    % wxWindowCreate
    wxFrame:connect(FrameB, create),

    % wxWindowDestroy
    wxFrame:connect(FrameB, destroy),
    % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    % The text for wxFrame:setStatusText/3 appears at the bottom of the window.
    % See http://erlang.org/doc/man/wxFrame.html#setStatusText-2
    ok = wxFrame:setStatusText(FrameB, "Mobile simulation status", []),

    % Get the current client drawing area.
    { Wclient, Hclient } = wxWindow:getClientSize(FrameB),
    io:format("Client size = (~p, ~p)~n", [Wclient, Hclient]),

    % Return the newly created frame.
    FrameB.
% End of createFrameB/1.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Redraw the window from the current display list.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
drawWindowB(_FrameB, DCclient, Dmap, Vars)
        when is_map(Dmap) andalso is_map(Vars) ->
    % Get the current client drawing area.
    % See http://erlang.org/doc/man/wxBufferedDC.html
    % http://docs.wxwidgets.org/3.0/classwx_buffered_d_c.html
    DCbuf = wxBufferedDC:new(DCclient),

    % Create a white brush for the background.
    BrushBG = wxBrush:new({255, 255, 255}),

    % Create a brush for the Arena of Action.
    BrushArena = wxBrush:new(?ARENA_BDY_COLOUR),

    % Create the brush for the nodes, with default colour if not found.
    % http://erlang.org/doc/man/maps.html#get-3
    ColNode = maps:get(nodeColour, Vars, ?NODE_COLOUR_DEFT),
    BrushCircle = wxBrush:new(ColNode),

    % Node shape and radius.
    NodeShape = maps:get(nodeShape, Vars, ?NODE_SHAPE_DEFT),
    NodeRadius = maps:get(nodeRadius, Vars, ?NODE_RAD_DEFT),

    wxBufferedDC:setBackground(DCbuf, BrushBG),
    wxBufferedDC:clear(DCbuf),

    % Draw the Arena boundary.
%    { Wclient, Hclient } = wxWindow:getClientSize(FrameB),
%    io:format("Client size = (~p, ~p)~n", [Wclient, Hclient]),
    {Wclient, Hclient} = maps:get(sizeClient, Vars, ?ARENA_SIZE_INIT),

    { ArenaPosX, ArenaPosY, ArenaPosW, ArenaPosH } =
        calcArenaXYWH({Wclient, Hclient}),

    wxBufferedDC:setBrush(DCbuf, BrushArena),
    wxDC:drawRectangle(DCbuf, {ArenaPosX, ArenaPosY}, {ArenaPosW, ArenaPosH}),

    % Draw the nodes.
    wxBufferedDC:setBrush(DCbuf, BrushCircle),

    % Pre-compute pentagon/hexagon dimensions to improve efficiency. (?)
    case NodeShape of
        nodeShapePentagon ->
            Cos72 = (math:sqrt(5.0) - 1.0)/4.0,
            Sin72 = math:sqrt(1.0 - Cos72 * Cos72),
            % Probably multiplication is faster than the square root.
%            Cos36 = (math:sqrt(5.0) + 1.0)/4.0,
%            Sin36 = math:sqrt(1.0 - Cos36 * Cos36),
            Cos36 = Sin72 * Sin72 * 2 - 1,
            Sin36 = Cos72 * Sin72 * 2,

            Rcos36 = ceil(NodeRadius * Cos36 + 0.5) - 1,
            Rsin36 = ceil(NodeRadius * Sin36 + 0.5) - 1,
            Rcos72 = ceil(NodeRadius * Cos72 + 0.5) - 1,
            Rsin72 = ceil(NodeRadius * Sin72 + 0.5) - 1,

            % Keep the compiler happy.
            Rcos60 = 0.0,
            Rsin60 = 0.0;
        nodeShapeHexagon ->
            Cos60 = 0.5,
            Sin60 = math:sqrt(0.75),

            % Note: math:floor() returns float, floor() returns integer.
%            Rcos60 = floor(NodeRadius * Cos60 + 0.5),
%            Rsin60 = floor(NodeRadius * Sin60 + 0.5),
            Rcos60 = ceil(NodeRadius * Cos60 + 0.5) - 1,
            Rsin60 = ceil(NodeRadius * Sin60 + 0.5) - 1,

            % Keep the compiler happy.
            Rcos36 = 0.0,
            Rsin36 = 0.0,
            Rcos72 = 0.0,
            Rsin72 = 0.0;
        _Else ->
            % http://erlang.org/doc/reference_manual/expressions.html#id80984
            % If I don't define Rcos60 and Rsin60 here, I get errors.
            % mobsim3.erl:747: variable 'Rcos60' unsafe in 'case' (line 708)
            % mobsim3.erl:747: variable 'Rsin60' unsafe in 'case' (line 708)
            % mobsim3.erl:748: variable 'Rcos60' unsafe in 'case' (line 708)
            % mobsim3.erl:748: variable 'Rsin60' unsafe in 'case' (line 708)
            % mobsim3.erl:750: variable 'Rcos60' unsafe in 'case' (line 708)
            % mobsim3.erl:750: variable 'Rsin60' unsafe in 'case' (line 708)
            % mobsim3.erl:751: variable 'Rcos60' unsafe in 'case' (line 708)
            % mobsim3.erl:751: variable 'Rsin60' unsafe in 'case' (line 708)
            % If I do define Cos60 and Sin60 here, I get this.
            % mobsim3.erl:718: Warning: variable 'Cos60' is unused
            % mobsim3.erl:719: Warning: variable 'Sin60' is unused
            % The compiler is too clever by half!
            Rcos36 = 0.0,
            Rsin36 = 0.0,
            Rcos60 = 0.0,
            Rsin60 = 0.0,
            Rcos72 = 0.0,
            Rsin72 = 0.0
        end,

    % - - - - - - - - - - - - - - - - - - -
    % I really want maps:foreach/2 here, but it doesn't exist.
    % http://erlang.org/doc/man/maps.html#fold-3
    % http://erlang.org/doc/man/lists.html#foreach-2
    FnD = fun(P, { Xold, Yold, Xnew, Ynew, Nevt }, AccIn) ->
%        PIDstring = pid_to_list(P),          % Example: "<18000.879.0>".
%        PIDstring = pidNoBrackets(P),        % Example: "18000.879.0".
        PIDstring = pidComponent(P, 2),     % Example: "879".
        wxDC:drawLine(DCbuf, {Xold, Yold}, {Xnew, Ynew}),
        case NodeShape of
            nodeShapeCircle ->
                % http://erlang.org/doc/man/wxDC.html#drawCircle-3
                wxDC:drawCircle(DCbuf, {Xnew, Ynew}, NodeRadius),
                wxDC:drawPoint(DCbuf, {Xnew, Ynew});
            nodeShapeSquare ->
                % http://erlang.org/doc/man/wxDC.html#drawRectangle-2
                wxDC:drawRectangle(DCbuf, {Xnew - NodeRadius, Ynew - NodeRadius,
                    2 * NodeRadius, 2 * NodeRadius}),
                wxDC:drawPoint(DCbuf, {Xnew, Ynew});
            nodeShapePentagon ->
                % http://erlang.org/doc/man/wxDC.html#drawPolygon-2
                Pts = [{Xnew, Ynew - NodeRadius},
                   {Xnew + Rsin72, Ynew - Rcos72},
                   {Xnew + Rsin36, Ynew + Rcos36},
                   {Xnew - Rsin36, Ynew + Rcos36},
                   {Xnew - Rsin72, Ynew - Rcos72}],
                wxDC:drawPolygon(DCbuf, Pts),
                wxDC:drawPoint(DCbuf, {Xnew, Ynew});
            nodeShapeHexagon ->
                % http://erlang.org/doc/man/wxDC.html#drawPolygon-2
                Pts = [{Xnew - NodeRadius, Ynew},
                   {Xnew - Rcos60, Ynew + Rsin60},
                   {Xnew + Rcos60, Ynew + Rsin60},
                   {Xnew + NodeRadius, Ynew},
                   {Xnew + Rcos60, Ynew - Rsin60},
                   {Xnew - Rcos60, Ynew - Rsin60}],
                wxDC:drawPolygon(DCbuf, Pts),
                wxDC:drawPoint(DCbuf, {Xnew, Ynew});
            nodeShapeX ->
                wxDC:drawLine(DCbuf, {Xnew - NodeRadius, Ynew + NodeRadius},
                    {Xnew + NodeRadius, Ynew - NodeRadius}),
                wxDC:drawLine(DCbuf, {Xnew - NodeRadius, Ynew - NodeRadius},
                    {Xnew + NodeRadius, Ynew + NodeRadius});
            true ->
                ok
            end,
        % NOTE. Make a rough guess of best string location. Fix this later!
        wxDC:drawText(DCbuf,
            PIDstring ++ " [" ++ integer_to_list(Nevt) ++ "]",
            {Xnew + 3, Ynew + 3}),
        AccIn
        end,
    maps:fold(FnD, 0, Dmap),

    % Destroy the brushes.
    wxBrush:destroy(BrushCircle),
    wxBrush:destroy(BrushArena),
    wxBrush:destroy(BrushBG),

    % Destroying the BufferedDC transfers the buffer to the ClientDC.
    wxBufferedDC:destroy(DCbuf),
    ok.
% End of drawWindowB/4.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Send window client size update to mobile nodes if the size has changed.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% In the wx_object framework, the server sends messages directly to mobiles,
% but the mobiles must send messages via wx:object:cast/2.
% http://erlang.org/doc/man/wx_object.html#cast-2
% If the mobile nodes are all ported to the gen_server framework, they will
% also need to receive cast/2 messages instead of direct send-operations.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sendSizeUpdateToMobs(Dmap, Vars, {WclientNew, HclientNew}) ->
    % Fetch the old size from the event handler's local variables.
    {WclientOld, HclientOld} = maps:get(sizeClient, Vars, ?ARENA_SIZE_INIT),

    if {WclientOld, HclientOld} /= {WclientNew, HclientNew} ->
        NewArena = calcArena({WclientNew, HclientNew}),
        FnD = fun(PidMobile, _, AccIn) ->
%            PidMobile ! { self(), sizeUpdate, {WclientNew, HclientNew} },
            PidMobile ! { self(), arenaUpdate, NewArena },
            AccIn
        end,
        maps:fold(FnD, 0, Dmap);
    true -> ok
    end.
% End of sendSizeUpdateToMobs/3.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Handle window events, plus messages from mobile nodes.
% This function will only receive #wx events.
% See file: /usr/local/lib/erlang/lib/wx-1.8.3/src/wx_object.erl
%
% dispatch({'$gen_cast', Msg}, Mod, State) ->
%     Mod:handle_cast(Msg, State);
% dispatch(Msg = #wx{}, Mod, State) ->
%     Mod:handle_event(Msg, State);
% dispatch(Info, Mod, State) ->
%     Mod:handle_info(Info, State).
%
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handleEventB(Evt, { FrameB, DCclient, Dmap, Vars })
        when is_map(Dmap) andalso is_map(Vars) ->
    case Evt of
        % Try to catch all events in a single case.
        #wx{id=Id, obj=Obj, event=EvtB} ->
            % Trace all messages.
%            io:format("Process ~p event id=~p, obj=~p, event=~n ~p~n",
%                [self(), Id, Obj, EvtB]),

            % Each case must return carry_on or exit_normal.
            % Alternatively let CarryOn equal a new version of Vars.
            % This might seem bizarre, but what else can you do if all
            % variables are constant?
            CarryOn = case EvtB of
            #wxClose{type=TypeB} ->
                TraceWindow = maps:get(traceWindow, Vars, ?TRACE_WINDOW_DEFT),
                case TypeB of
                % This is the event which is called when the window is closed.
                close_window ->
                    if TraceWindow ->
                        io:format("Process ~p closing window id=~p, obj=~p~n",
                            [self(), Id, Obj]);
                    true -> ok
                    end,
                    wxFrame:setStatusText(FrameB, "Closing now...", []),
                    wxWindow:destroy(FrameB),
%                    exit(normal);
                    exit_normal;
                % This event is never received!?
                end_session ->
                    if TraceWindow ->
                        io:format("Process ~p ending session id=~p, obj=~p~n",
                            [self(), Id, Obj]);
                    true -> ok
                    end,
                    wxFrame:setStatusText(FrameB, "Ending session now...", []),
                    wxWindow:destroy(FrameB),
%                    exit(normal);
                    exit_normal;
                _Else ->
                    carry_on
                end;

            #wxActivate{type=TypeB, active=Ac} ->
                TraceWindow = maps:get(traceWindow, Vars, ?TRACE_WINDOW_DEFT),
                if TraceWindow ->
                    case TypeB of
                    activate ->
                        io:format("window activate: active=~p~n", [Ac]);
                    activate_app ->
                        io:format("window activate app: active=~p~n", [Ac]);
                    hibernate ->
                        io:format("window hibernate: active=~p~n", [Ac]);
                    _Else ->
                        ok
                    end;
                true -> ok
                end,
                carry_on;

            #wxCommand{type=TypeB,
                    cmdString=Cstring, commandInt=Cint, extraLong=Elong} ->
                % http://erlang.org/doc/man/wxEvtHandler.html#type-wxCommand
                % http://erlang.org/doc/man/wxCommandEvent.html
                % http://docs.wxwidgets.org/2.8.12/wx_wxcommandevent.html
                TraceMenu = maps:get(traceMenu, Vars, ?TRACE_MENU_DEFT),
                case TypeB of
                command_menu_selected ->
                    if TraceMenu ->
                        io:format("menu item selected: "
                           "id=~p, cmdString=~p, commandInt=~p, extraLong=~p~n",
                            [Id, Cstring, Cint, Elong]);
                    true -> ok
                    end,
                    case Id of
                        % http://docs.wxwidgets.org/2.8.12/wx_stdevtid.html
                        ?wxID_EXIT ->
                            if TraceMenu ->
                                io:format("Exit menu item selected~n", []);
                            true -> ok
                            end,
                            exit_normal;

                        % Change the node colour.
                        ?MENU_ITEM_R ->
                            % http://erlang.org/doc/man/maps.html#put-3
                            maps:put(nodeColour, ?NODE_COLOUR_R, Vars);
                        ?MENU_ITEM_G ->
                            maps:put(nodeColour, ?NODE_COLOUR_G, Vars);
                        ?MENU_ITEM_B ->
                            maps:put(nodeColour, ?NODE_COLOUR_B, Vars);

                        ?MENU_ITEM_CIRCLE ->
                            maps:put(nodeShape, nodeShapeCircle, Vars);
                        ?MENU_ITEM_SQUARE ->
                            maps:put(nodeShape, nodeShapeSquare, Vars);
                        ?MENU_ITEM_PENTAGON ->
                            maps:put(nodeShape, nodeShapePentagon, Vars);
                        ?MENU_ITEM_HEXAGON ->
                            maps:put(nodeShape, nodeShapeHexagon, Vars);
                        ?MENU_ITEM_X ->
                            maps:put(nodeShape, nodeShapeX, Vars);

                        ?MENU_ITEM_RAD1 ->
                            maps:put(nodeRadius, ?NODE_RAD1, Vars);
                        ?MENU_ITEM_RAD2 ->
                            maps:put(nodeRadius, ?NODE_RAD2, Vars);
                        ?MENU_ITEM_RAD3 ->
                            maps:put(nodeRadius, ?NODE_RAD3, Vars);
                        ?MENU_ITEM_RAD4 ->
                            maps:put(nodeRadius, ?NODE_RAD4, Vars);

                        % Trace options.
                        ?MENU_ITEM_TRACE_DMAP ->
                            maps:put(traceDmap, int_to_boolean(Cint), Vars);
                        ?MENU_ITEM_TRACE_MOUSE ->
                            maps:put(traceMouse, int_to_boolean(Cint), Vars);
                        ?MENU_ITEM_TRACE_MOTION ->
                            maps:put(traceMotion, int_to_boolean(Cint), Vars);
                        ?MENU_ITEM_TRACE_CURSOR ->
                            maps:put(traceCursor, int_to_boolean(Cint), Vars);
                        ?MENU_ITEM_TRACE_WINDOW ->
                            maps:put(traceWindow, int_to_boolean(Cint), Vars);
                        ?MENU_ITEM_TRACE_MENU ->
                            maps:put(traceMenu, int_to_boolean(Cint), Vars);
                        ?MENU_ITEM_TRACE_UPDATE_UI ->
                            maps:put(traceUpdateUI, int_to_boolean(Cint), Vars);
                        ?MENU_ITEM_TRACE_NODE ->
                            maps:put(traceNode, int_to_boolean(Cint), Vars);

                        _Else ->
                            carry_on
                    end;
                _Else ->
                    if TraceMenu ->
                        io:format("command event ~p: "
                           "id=~p, cmdString=~p, commandInt=~p, extraLong=~p~n",
                            [TypeB, Id, Cstring, Cint, Elong]);
                    true -> ok
                    end,
                    carry_on
                end;

            #wxIconize{type=TypeB, iconized=Ic} ->
                % Trace window events.
                TraceWindow = maps:get(traceWindow, Vars, ?TRACE_WINDOW_DEFT),
                case TypeB of
                iconize ->
                    if TraceWindow ->
                        io:format("window iconize: iconized=~p~n", [Ic]);
                    true -> ok
                    end;
                _Else ->
                    ok
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

            #wxMenu{type=TypeB, menuId=Id, menu=Menu} ->
                % Trace menu events.
                TraceMenu = maps:get(traceMenu, Vars, ?TRACE_MENU_DEFT),
                case TypeB of
                menu_open ->
                    if TraceMenu ->
                        io:format("menu open: menuId=~p, menu=~p~n",
                            [Id, Menu]);
                    true -> ok
                    end;
                menu_close ->
                    if TraceMenu ->
                        io:format("menu close: menuId=~p, menu=~p~n",
                            [Id, Menu]);
                    true -> ok
                    end;
                menu_highlight ->
                    if TraceMenu ->
                        io:format("menu highlight: menuId=~p, menu=~p~n",
                            [Id, Menu]);
                    true -> ok
                    end;
                _Else ->
                    if TraceMenu ->
                        io:format("unknown menu event: menuId=~p, menu=~p~n",
                            [Id, Menu]);
                    true -> ok
                    end
                end,
                carry_on;

            #wxMouse{type=TypeB, x=X, y=Y,
                    leftDown=L, middleDown=M, rightDown=R, controlDown=C,
                    shiftDown=S, altDown=A, metaDown=Meta, wheelRotation=Wrot,
                    wheelDelta=Wdelta, linesPerAction=LPA} ->
                TraceMouse = maps:get(traceMouse, Vars, ?TRACE_MOUSE_DEFT),
                TraceMotion = maps:get(traceMotion, Vars, ?TRACE_MOTION_DEFT),
                case TypeB of
                motion ->
                    if TraceMotion ->
                        io:format("mouse motion: "
                            "X=~p, Y=~p~n", [X, Y]);
                    true -> ok
                    end;
                left_down ->
                    if TraceMouse ->
                        io:format("mouse left down: "
                            "X=~p, Y=~p, L=~p, M=~p, R=~p,~n C=~p, S=~p, A=~p"
                            ", Meta=~p, Wrot=~p, Wdelta=~p, LPA=~p~n",
                            [X, Y, L, M, R, C, S, A, Meta, Wrot, Wdelta, LPA]);
                    true -> ok
                    end;
                left_up ->
                    if TraceMouse ->
                    io:format("mouse left up: "
                            "X=~p, Y=~p, L=~p, M=~p, R=~p,~n C=~p, S=~p, A=~p"
                            ", Meta=~p, Wrot=~p, Wdelta=~p, LPA=~p~n",
                            [X, Y, L, M, R, C, S, A, Meta, Wrot, Wdelta, LPA]);
                    true -> ok
                    end;
                _Else ->
                    if TraceMouse ->
                    io:format("mouse event: type=~p, "
                        "X=~p, Y=~p~n", [TypeB, X, Y]);
                    true -> ok
                    end
                end,
                carry_on;

            % The returned rectangle is always {0, 0, 0, 0}. Not good!
            #wxMove{type=TypeB, pos={Ws, Hs}, rect={Xr, Yr, Wr, Hr}} ->
                % Trace window events.
                TraceWindow = maps:get(traceWindow, Vars, ?TRACE_WINDOW_DEFT),
                if TraceWindow ->
                    case TypeB of
                    move ->
                        io:format("window ~p move: "
                            "pos=[~p, ~p], rect: X=~p, Y=~p, W=~p, H=~p~n",
                            [Id, Ws, Hs, Xr, Yr, Wr, Hr]);
                    _Else ->
                        io:format("window ~p move: unknown type = ~p: "
                            "pos=[~p, ~p], rect: X=~p, Y=~p, W=~p, H=~p~n",
                            [Id, TypeB, Ws, Hs, Xr, Yr, Wr, Hr])
                    end;
                true -> ok
                end,
                carry_on;

            #wxPaint{type=TypeB} ->
                % Trace window events.
                TraceWindow = maps:get(traceWindow, Vars, ?TRACE_WINDOW_DEFT),
                case TypeB of
                paint ->
                    if TraceWindow ->
                        io:format("window paint event=~p~n", [EvtB]);
                    true -> ok
                    end,
                    % Create temporary context for the refresh.
                 % http://docs.wxwidgets.org/2.8.12/wx_wxpaintdc.html#wxpaintdc
                    DCpaint = wxPaintDC:new(FrameB),

                    % Draw all of the nodes in the display list.
                    drawWindowB(FrameB, DCpaint, Dmap, Vars),

                    % Clean out the trash.
                    wxPaintDC:destroy(DCpaint);
                _Else ->
                    if TraceWindow ->
                        io:format("window paint event: unknown type=~p ",
                            [TypeB]);
                    true -> ok
                    end
                end,
                carry_on;

            #wxSetCursor{type=TypeB, x=X, y=Y, cursor=Cursor} ->
                TraceCursor = maps:get(traceCursor, Vars, ?TRACE_CURSOR_DEFT),
                case TypeB of
                set_cursor ->
                    if TraceCursor ->
                        io:format("set-cursor event: "
                            "X=~p, Y=~p, cursor=~p~n", [X, Y, Cursor]);
                    true -> ok
                    end;
                _Else ->
                    if TraceCursor ->
                    io:format("unknown set-cursor event: type=~p, "
                        "X=~p, Y=~p, cursor=~p~n", [TypeB, X, Y, Cursor]);
                    true -> ok
                    end
                end,
                carry_on;

            % The returned rectangle is always {0, 0, 0, 0}. Not good!!
            #wxSize{type=TypeB, size={Ws, Hs}, rect={Xr, Yr, Wr, Hr}} ->
                TraceWindow = maps:get(traceWindow, Vars, ?TRACE_WINDOW_DEFT),
                case TypeB of
                size ->
                    { WclientNew, HclientNew } = wxWindow:getClientSize(FrameB),
                    if TraceWindow ->
                        io:format("window ~p size: "
                            "size=[~p, ~p], rect: X=~p, Y=~p, W=~p, H=~p~n",
                            [Id, Ws, Hs, Xr, Yr, Wr, Hr]),
                        io:format("    client window ~p size: {~p, ~p}~n",
                            [Id, WclientNew, HclientNew]);
                    true -> ok
                    end,

                    % Send news to the mobile nodes.
                    sendSizeUpdateToMobs(Dmap, Vars, {WclientNew, HclientNew}),

                    % Record the new window area dimensions in Vars.
                    Vars1 = maps:put(sizeWindow, {Ws, Hs}, Vars),

                    % Record the new client area dimensions in Vars.
                    maps:put(sizeClient, {WclientNew, HclientNew}, Vars1);
                _Else ->
                    if TraceWindow ->
                        io:format("window size: unknown type = ~p: "
                            "size=[~p, ~p], rect: X=~p, Y=~p, W=~p, H=~p~n",
                            [TypeB, Ws, Hs, Xr, Yr, Wr, Hr]);
                    true -> ok
                    end,
                    carry_on
                end;

            #wxShow{type=TypeB} ->
                TraceWindow = maps:get(traceWindow, Vars, ?TRACE_WINDOW_DEFT),
                case TypeB of
                show ->
                    if TraceWindow ->
                        io:format("window show~n");
                    true -> ok
                    end;
                _Else ->
                    if TraceWindow ->
                        io:format("window show: unknown type=~p ", [TypeB]);
                    true -> ok
                    end
                end,
                carry_on;

            % http://erlang.org/doc/man/wxEvtHandler.html#type-wxUpdateUI
            #wxUpdateUI{type=TypeB} ->
                TraceUpdateUI
                    = maps:get(traceUpdateUI, Vars, ?TRACE_UPDATE_UI_DEFT),
                case TypeB of
                update_ui ->
                    if TraceUpdateUI ->
                        io:format("Update UI event: id=~p~n", [Id]);
                    true -> ok
                    end;
                _Else ->
                    if TraceUpdateUI ->
                        io:format("unknown Update UI event: type=~p, id=~p~n",
                            [TypeB, Id]);
                    true -> ok
                    end
                end,
                carry_on;

            #wxWindowCreate{type=TypeB} ->
                TraceWindow = maps:get(traceWindow, Vars, ?TRACE_WINDOW_DEFT),
                case TypeB of
                create ->
                    if TraceWindow ->
                        io:format("window create~n");
                    true -> ok
                    end;
                _Else ->
                    if TraceWindow ->
                        io:format("window create: unknown type=~p ", [TypeB]);
                    true -> ok
                    end
                end,
                carry_on;

            #wxWindowDestroy{type=TypeB} ->
                TraceWindow = maps:get(traceWindow, Vars, ?TRACE_WINDOW_DEFT),
                case TypeB of
                destroy ->
                    if TraceWindow ->
                        io:format("window destroy~n");
                    true -> ok
                    end;
                _Else ->
                    if TraceWindow ->
                        io:format("window destroy: unknown type=~p ", [TypeB]);
                    true -> ok
                    end
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
                    { noreply, { FrameB, DCclient, Dmap, Vars } };
                CarryOn == exit_normal ->
                    io:format("~p wx event handler normal exit~n", [self()]),
                    % http://erlang.org/doc/man/wx_object.html
                    { stop, normal, { FrameB, DCclient, Dmap, Vars } };
                is_map(CarryOn) ->
                    % This kind of event is usually a menu selection.
                    % So only trace it when menu events are being traced.
                    % The compiler complains if TraceMenu is used here.
                    TraceMenu2 = maps:get(traceMenu, Vars, ?TRACE_MENU_DEFT),
                    if TraceMenu2 ->
                        io:format("~p event handler parameter changed~n",
                            [self()]);
                    true -> ok
                    end,
                    { noreply, { FrameB, DCclient, Dmap, CarryOn} };
                true ->
                    io:format("~p wx event handler abnormal exit: CarryOn=~p~n",
                        [self(), CarryOn]),
                    { stop, shutdown }
            end;

        % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        % All other event classes which are connected.
        _Else ->
            io:format("Process ~p received unknown event ~p~n", [self(), Evt]),
            % Loop around and do it all again.
            { noreply, { FrameB, DCclient, Dmap, Vars } }
    end.
% End of handleEventB/2.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Handle window events, plus messages from mobile nodes.
% This function will only receive cast events, never #wx events.
% See file: /usr/local/lib/erlang/lib/wx-1.8.3/src/wx_object.erl
%
% dispatch({'$gen_cast', Msg}, Mod, State) ->
%     Mod:handle_cast(Msg, State);
% dispatch(Msg = #wx{}, Mod, State) ->
%     Mod:handle_event(Msg, State);
% dispatch(Info, Mod, State) ->
%     Mod:handle_info(Info, State).
%
% These events are expected from mobile nodes.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% In the wx_object framework, the server sends messages directly to mobiles,
% but the mobiles must send messages via wx:object:cast/2.
% http://erlang.org/doc/man/wx_object.html#cast-2
% If the mobile nodes are all ported to the gen_server framework, they will
% also need to receive cast/2 messages instead of direct send-operations.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
handleCastB(Evt, { FrameB, DCclient, Dmap, Vars })
        when is_map(Dmap) andalso is_map(Vars) ->
    TraceNode = maps:get(traceNode, Vars, ?TRACE_NODE_DEFT),

    % Trace all cast messages.
    if TraceNode ->
%         io:format("Process ~p cast event=~p~n", [self(), Evt]);
        ok;
    true -> ok
    end,
    case Evt of
        % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        % Handle position message from a mobile client.
        { PIDclient, pos, Ntimes, { Xold, Yold, Xnew, Ynew }} ->
            if TraceNode ->
                io:format("~p received position event ~p from ~p: "
                    "(~p,~p,~p,~p)~n",
                    [self(), Ntimes, PIDclient, Xold, Yold, Xnew, Ynew]);
            true -> ok
            end,
            % Confirm receipt of the data.
            PIDclient ! { self(), pos_resp, Ntimes },

            % Update the display list.
            % Two equivalent ways to put a key/value in the map.
%            DmapNew = Dmap#{ PIDclient => { Xold, Yold, Xnew, Ynew, Ntimes }},
            DmapNew = maps:put(PIDclient,
                { Xold, Yold, Xnew, Ynew, Ntimes }, Dmap),
            % See also http://erlang.org/doc/man/maps.html#size-1
            Nmobs = erlang:map_size(DmapNew),
            StrNmobs = integer_to_list(Nmobs),

            % Status text at the bottom of the window.
            % See http://erlang.org/doc/man/wxFrame.html#setStatusText-2
            ok = wxFrame:setStatusText(FrameB,
                "Status: N mobiles = " ++ StrNmobs, []),

            % Trace the display-list.
            TraceDmap = maps:get(traceDmap, Vars, ?TRACE_DMAP_DEFT),
            if TraceDmap ->
                    io:format("~p new display list: ~p~n", [self(), DmapNew]);
            true -> ok
            end,

            % Draw all of the nodes in the display list.
            drawWindowB(FrameB, DCclient, DmapNew, Vars),

            % Loop around and do it all again.
            { noreply, { FrameB, DCclient, DmapNew, Vars } };

        % Handle finish message from a mobile client.
        { PIDclient, fin, Ntimes, { Xold, Yold, Xnew, Ynew }} ->
            if TraceNode ->
                io:format("~p received finish event ~p from ~p: "
                    "(~p,~p,~p,~p)~n",
                    [self(), Ntimes, PIDclient, Xold, Yold, Xnew, Ynew]);
            true -> ok
            end,

            % Confirm receipt of the data.
            PIDclient ! { self(), fin_resp, Ntimes },

            % Update the display list.
            DmapNew = maps:remove(PIDclient, Dmap),
            Nmobs = erlang:map_size(DmapNew),
            StrNmobs = integer_to_list(Nmobs),

            % Status text at the bottom of the window.
            % See http://erlang.org/doc/man/wxFrame.html#setStatusText-2
            ok = wxFrame:setStatusText(FrameB,
                "Status: N mobiles = " ++ StrNmobs, []),

            % Trace the display-list.
            TraceDmap = maps:get(traceDmap, Vars, ?TRACE_DMAP_DEFT),
            if TraceDmap ->
                    io:format("~p new display list: ~p~n", [self(), DmapNew]);
            true -> ok
            end,

            % Draw all of the nodes in the display list.
            drawWindowB(FrameB, DCclient, DmapNew, Vars),

            % Loop around and do it all again.
            { noreply, { FrameB, DCclient, DmapNew, Vars } };

        % Handle request for client window size from a mobile client.
        { PIDclient, req_arena_size } ->
            if TraceNode ->
                io:format("~p received client size request from ~p~n",
                    [self(), PIDclient]);
            true -> ok
            end,

            % Reply with the requested client window size.
            { Wclient, Hclient } = maps:get(sizeClient, Vars, ?ARENA_SIZE_INIT),
            Arena = calcArena({Wclient, Hclient}),

%            PIDclient ! { self(), sizeUpdate, { Wclient, Hclient } },
            PIDclient ! { self(), arenaUpdate, Arena },

            % Loop around and do it all again.
            { noreply, { FrameB, DCclient, Dmap, Vars } };

        % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        % All other event classes which are connected.
        _Else ->
            io:format("Process ~p received unknown event ~p~n", [self(), Evt]),
            % Loop around and do it all again.
            { noreply, { FrameB, DCclient, Dmap, Vars } }
    end.
% End of handleCastB/2.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This function is intended to be called from mobsim4b:init/1.
% It does not call the event handler for the window process.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
initWindowB() ->
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

    % Initialize state variables and return them.
    Dmap = #{},
    Vars = #{},
    { FrameB, DCclient, Dmap, Vars }.
% End of initWindowB/0.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This function is called from the process managing the wx_object process.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
termWindowB(_Reason, { _FrameB, DCclient, _Dmap, _Vars }) ->
    % Destroy the Brush.
    % Can't destroy the brush directly. It will be destroyed by wxClientDC.
%    io:format("Destroy background Brush~n", []),
%    wxBrush:destroy(BrushBG),

    % Destroy the Device Context.
    % NOTE: This is almost certainly unnecessary.
    io:format("Destroy DC (Device Context)~n", []),
    wxClientDC:destroy(DCclient),

    % Destroy the wx server.
    % There can only be one, I think. So there's no need to specify which one.
    % NOTE: This is almost certainly superfluous.
    io:format("Destroy wx server~n", []),
    wx:destroy(),
    ok.
% End of termWindowB/2.
