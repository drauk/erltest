% src/erlang/test1.erl   2018-2-25   Alan U. Kennington.
% Test run of erlang programming language to see what it can do.
% Based on http://erlang.org/doc/getting_started/seq_prog.html

-module(test1).

-export([double/1]).
-export([f/1, facto/1, factoTR/1]).
-export([tconv/3]).
-export([newcol/4, blendcol/2]).
-export([listmax/1]).
-export([listrev/1]).
-export([monthdays/2]).

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

%==============================================================================
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Factorial of a positive integer.
% Naive version without an if-clause.
% Test:
% >>> test1:f(5).
% 120
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The function name "f1" is not permitted.
% Error was:
%   {error,non_existing}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This is not "tail recursive" because the last expression is not "f(N-1)".
% The current context must therefore be maintained while evaluating the
% expression "N * f(N-1)" by invoking a new context.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
f(1) ->
    1;
f(N) ->
    N * f(N-1).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Single-function factorial function for non-negative integer.
% Test:
% >>> test1:facto(5).
% 120
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This is not "tail recursive" because the last expression is not "facto(N-1)".
% The current context must therefore be maintained while evaluating the
% expression "N * facto(N-1)" by invoking a new context.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
facto(N) when is_integer(N) andalso N >= 0 ->
    if
        N == 0 ->
            1;
        true ->
            N * facto(N-1)
    end.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Factorial functions which is (hopefully) tail-recursive.
% Requires one trivial starter-function.
% Test:
% >>> test1:factoTR(5).
% 120
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
factoTR(N, Prod) when is_integer(N) andalso N >= 0 andalso is_number(Prod) ->
    if
        N == 0 ->
            Prod;
        true ->
            factoTR(N - 1, Prod * N)
    end.
factoTR(N) when is_integer(N) andalso N >= 0 ->
    factoTR(N, 1).

%==============================================================================
% Convert temperature.
% An exercise for labels, which are called "atoms".
% See: http://erlang.org/doc/reference_manual/data_types.html#id66121
%  "An atom is to be enclosed in single quotes (') if it does not begin with a
%  lower-case letter or if it contains other characters than alphanumeric
%  characters, underscore (_), or @."
tconv(T, c, f) ->
    (T * 9) / 5 + 32;
tconv(T, f, c) ->
    (T - 32) / 9 * 5.

%==============================================================================
% Colour-blending functions to demonstrate maps.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% This is the test program for newcol/4 and blendcol/2.
% Note that C1 is 100% opaque here.
%
% >>> C1 = test1:newcol(0.3,0.4,0.5,1.0).
% #{red => 0.3,green => 0.4,blue => 0.5,alpha => 1.0}
% >>> C2 = test1:newcol(1.0,0.8,0.1,0.3).
% #{red => 1.0,green => 0.8,blue => 0.1,alpha => 0.3}
% >>> test1:blendcol(C1,C2).
% #{red => 0.3,green => 0.4,blue => 0.5,alpha => 1.0}
% >>> test1:blendcol(C2,C1).
% #{red => 0.51,green => 0.52,blue => 0.38,alpha => 1.0}
%
% Summary of bracket styles.
% {1, 2, 3}             Tuple.
% [1, 2, 3]             List.
% #{ 1 => 10, 2 => 11 } Map.
% "123"                 List.

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
% These assignments suggest that maps are not immutable!
% This is called "updating maps".
% However, it is just a local copy of the _value_ of the map.
% See http://erlang.org/doc/reference_manual/expressions.html#map_expressions
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
blendcol(S, D, A) when A > 0.0 ->
    D#{
        red := red_blend(S,D)/A,
        green := green_blend(S,D)/A,
        blue := blue_blend(S,D)/A,
        alpha := A
        };
blendcol(_S, D, _A) ->
    D#{ red := 0.0, green := 0.0, blue := 0.0, alpha := 0.0 }.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The two-parameter export version.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
blendcol(S, D) ->
    blendcol(S, D, alpha_blend(S, D)).

%==============================================================================
% List maximum function.
% Lists: http://erlang.org/doc/reference_manual/data_types.html#id77524
% Lists library: http://erlang.org/doc/man/lists.html
% The maximum of the empty list is undefined. (Not a bug!)
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Test:
% (clientD@hostA)206> test1:listmax([7,6,5,1,4,54,64.2,3]).
% 64.2
% (clientD@hostA)207> test1:listmax([3]).
% 3
% (clientD@hostA)208> test1:listmax([]).
% ** exception error: no function clause matching test1:listmax([]) (test1.erl,
% line 218)

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
%   test1.erl:159: Warning: variable 'Xunused' is unused
% It's very lucky that this is not a fatal error as it always is in Golang.
% Erlang would be totally unusable if unused variables were forbidden.
% listmax([Xunused | L], Y) ->
listmax([_X | L], Y) ->
    listmax(L, Y).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The one-parameter export version.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
listmax([X | L]) when is_list(L) ->
    listmax(L, X).

%==============================================================================
% List reversal function.
% Test:
% >>> test1:listrev([-1, 7, 9.2, -3.3]).
% [-3.3,9.2,7,-1]

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The two-parameter version.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Move each element X so that it is before all previously removed elements R.
listrev([X | L], R) ->
    listrev(L, [X | R]);
listrev([], R) ->
    R.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% The one-parameter export version.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
listrev(L) ->
    listrev(L, []).

%==============================================================================
% Compute the number of days in a given year/month.
% Test:
% >>> test1:monthdays(2018, 2).
% 28
% >>> test1:monthdays(2016, 2).
% 29

% In the if-clause for monthdays/2, the function is_div/2 can't be used.
% test1.erl:201: call to local/imported function is_div/2 is illegal in guard.
% I don't know where this arbitrary-looking rule comes from.
-define(hide_is_div_unused, true).
-ifndef(hide_is_div_unused).
is_div_unused(X, N) ->
    if is_float(N) andalso N > 0 ->
        false;
    true ->
        trunc(X/N) * N == X
    end.
-endif.

% An explanation is here:
% http://erlang.org/doc/getting_started/seq_prog.html#id68896
% Only a few BIFs can be used in guards, and you cannot use functions you have
% defined yourself in guards. (see Guard Sequences)
% (For advanced readers: This is to ensure that guards do not have side
% effects.)
%
% That makes sense, I guess.

% The following macro is also not accepted in the if-clause for Leapmonth.
% test1.erl:206: illegal guard expression
-define(is_div(X, N), (is_float(N) andalso N > 0 andalso trunc(X/N) * N == X)).
% -define(is_div(X, N), (is_float(N) and N > 0 and trunc(X/N) * N == X)).

% This function doesn't work before about October 1582 in Spain,
% nor before 14 September 1752 in Britain.
% See https://en.wikipedia.org/wiki/Gregorian_calendar#Adoption
monthdays(Y, M) ->
    Leapmonth = if
        Y rem 400 == Y ->
            leapT;
        Y rem 100 == Y ->
            leapF;
%        is_div(Y, 4) ->
        Y rem 4 == 0 ->
            leapT;
        true ->
            leapF
    end,

    case M of
        1 -> 31;
        2 when Leapmonth == leapT -> 29;
        2 -> 28;
        3 -> 31;
        4 -> 30;
        5 -> 31;
        6 -> 30;
        7 -> 31;
        8 -> 31;
        9 -> 30;
        10 -> 31;
        11 -> 30;
        12 -> 31
    end.
