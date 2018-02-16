% src/erlang/wx1.erl   2018-2-16   Alan U. Kennington.
% $Id$
% Familiarization with wxErlang library.
% Reference manual: http://erlang.org/doc/apps/wx/index.html
% See also file: /usr/local/lib/erlang/lib/wx-1.8.3/examples/simple/hello.erl

-module(wx1).

-include_lib("wx/include/wx.hrl").

-export([startWindowA/0]).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Do not use this function yet.
% It does everything incorrectly.
% Also, on my system it has problems with SCIM.
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
startWindowA() ->
    ServerA = wx:new(),
    wx:get_env(),
    erlang:ports(),
    FrameA = wxFrame:new(ServerA, -1, "test A", [{size, {600, 400}}]),
    wxWindow:show(FrameA).
