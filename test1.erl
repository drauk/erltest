% src/erlang/test1.erl   2018-2-12   Alan U. Kennington.
% $Id$
% Test run of erlang programming language to see what it can do.
% Based on http://erlang.org/doc/getting_started/seq_prog.html

-module(test1).
-export([double/1, f/1]).
-export([tconv/3]).
-export([newcol/4, blendcol/2]).
-export([listmax/1]).

%==============================================================================
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Double a number.
% The argument identifier must start with a capital letter.
% Error was:
%   test1.erl:5: Warning: this expression will fail with a 'badarith' exception
%   {ok,test1}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
double(Abc) ->
    2*Abc.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Factorial of a positive integer.
% The function name "f1" is not permitted.
% Error was:
%   {error,non_existing}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
f(1) ->
    1;
f(N) ->
    N * f(N-1).

%==============================================================================
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Convert temperature.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
tconv(T, c, f) ->
    (T * 9) / 5 + 32;
tconv(T, f, c) ->
    (T - 32) / 9 * 5.

%==============================================================================
% This is the test program for newcol/4 and blendcol/2.
% Note that C1 is 100% opaque here.
%
% 2> C1 = test1:newcol(0.3,0.4,0.5,1.0).
% #{red => 0.3,green => 0.4,blue => 0.5,alpha => 1.0}
% 3> C2 = test1:newcol(1.0,0.8,0.1,0.3).
% #{red => 1.0,green => 0.8,blue => 0.1,alpha => 0.3}
% 4> test1:blendcol(C1,C2).
% #{red => 0.3,green => 0.4,blue => 0.5,alpha => 1.0}
% 5> test1:blendcol(C2,C1).
% #{red => 0.51,green => 0.52,blue => 0.38,alpha => 1.0}

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Define a macro.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-define(is_colval(V), (is_float(V) andalso V >= 0.0 andalso V =< 1.0)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Colour/alpha merge function. Alpha = transparency. Alpha = 0 means opaque.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
newcol(R, G, B, A)
    when ? is_colval(R), ? is_colval(G), ? is_colval(B), ? is_colval(A) ->
    #{ red => R, green => G, blue => B, alpha => A }.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Alpha blend. I.e. opacity blend.
% Use "reverse assignments" to get SA and DA out of the arguments.
% New value is SA + DA - DA * SA, which is symmetric.
% This equals 1 - (1 - SA) * (1 - DA).
% I.e. output transparency = product of input transparencies.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
alpha_blend(#{ alpha := SA }, #{ alpha := DA }) ->
    SA + DA * (1.0 - SA).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Red blend. Assume that S is in front of D. Not symmetric.
% Extract the first argument's red and alpha values as SV and SA.
% Extract the second argument's red and alpha values as DV and DA.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
red_blend(#{ red := SV, alpha := SA }, #{ red := DV, alpha := DA }) ->
    (SV * SA) + (DV * DA) * (1.0 - SA).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Green blend. Assume that S is in front of D. Not symmetric.
% Extract the first argument's green and alpha values as SV and SA.
% Extract the second argument's green and alpha values as DV and DA.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
green_blend(#{ green := SV, alpha := SA }, #{ green := DV, alpha := DA }) ->
    (SV * SA) + (DV * DA) * (1.0 - SA).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Blue blend. Assume that S is in front of D. Not symmetric.
% Extract the first argument's blue and alpha values as SV and SA.
% Extract the second argument's blue and alpha values as DV and DA.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
blue_blend(#{ blue := SV, alpha := SA }, #{ blue := DV, alpha := DA }) ->
    (SV * SA) + (DV * DA) * (1.0 - SA).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The 3-parameter blendcol is local. The 2-parameter version is exported.
% These assignments show that maps are _not_ immutable!
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
blendcol(S, D, A) when A > 0.0 ->
    D#{
        red := red_blend(S,D)/A,
        green := green_blend(S,D)/A,
        blue := blue_blend(S,D)/A,
        alpha := A
        };
blendcol(_, D, _) ->
    D#{ red := 0.0, green := 0.0, blue := 0.0, alpha := 0.0 }.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The exported 2-parameter version.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
blendcol(S, D) ->
    blendcol(S, D, alpha_blend(S, D)).

%==============================================================================
% List maximum function.
% Test:
% 2> test1:listmax([7,6,5,1,4,54,64.2,3]).
% 64.2

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The two-parameter version.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% If the list has only one element Y, return it.
listmax([], Y) ->
    Y;

% If the first element Y is less than the second element X,
% remove Y from the list, and move X into the first position.
listmax([X | L], Y) when X > Y ->
    listmax(L, X);

% If the first element Y is _not_ less than the second element X,
% remove X from the list, and keep Y in the first position.
% NOTE: This gives a compilation warning because X is not used.
%   test1.erl:138: Warning: variable 'X' is unused
listmax([X | L], Y) ->
    listmax(L, Y).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The one-parameter version.
% Apparently the maximum of the empty list is undefined!
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
listmax([X | L]) ->
    listmax(L, X).
