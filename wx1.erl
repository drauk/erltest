% src/erlang/wx1.erl   2018-2-16   Alan U. Kennington.
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

    % I don't know what this does.
    % See http://erlang.org/doc/man/wxFrame.html
    % which says that wxFrame is derived from:
    % wxTopLevelWindow, wxWindow, wxEvtHandler
    % http://erlang.org/doc/man/wxEvtHandler.html#connect-2
    % wxCloseEventType() = close_window | end_session | query_end_session
    wxFrame:connect(FrameA, close_window),

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
    receive
        #wx{event=#wxClose{}} ->
            io:format("Process ~p closing window~n",[self()]),
            ok = wxFrame:setStatusText(FrameA, "Closing soon...", []),
            timer:sleep(5000),
            wxWindow:destroy(FrameA),
            ok;
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
    wxWindow:show(FrameA),

    % Go into a loop.
    handleWindowA(FrameA),

    % Wait for a while.
%    timer:sleep(5000),

    % Destroy everything.
    wx:destroy(),
    ok.
