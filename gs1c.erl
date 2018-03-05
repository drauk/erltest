% src/erlang/gs1c.erl   2018-3-6   Alan U. Kennington.
% Testing the Erlang/OTP gen_server concept.
% Here the user-interface and server processes are separate, to investigate
% what happens when the advice in the documentation is _not_ followed!

% Module C.
% This module provides some basic low-level "services" to Module B.
% The basic user access (client) functions are in Module A: gs1a.erl.
% The gen_server callback handlers are in Module B: gs1b.erl.
% The "real" services are in Module C.
% Call chain: Erlang shell <==> A <==> GS-module <- -> GS-daemon <==> B <==> C.

-module(gs1c).

-export([channels/0, alloc/1, free/2]).

%==============================================================================
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Constructor for a list of integers which can be allocated.
% The returned structure will be held in trust by the gen_server module.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
channels() ->
    Allocated = [],
    Free = lists:seq(1, 100),
    { Allocated, Free }.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Allocate one of the numbers in the free-list
% Return the allocated number and the new state.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
alloc({ Allocated, [X | Tail] = _Free }) ->
    NewState = { [X | Allocated], Tail },
    { X, NewState }.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Put a number back in the free-list.
% Return the new state.
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
free(X, { Allocated, Free } = State) ->
    case lists:member(X, Allocated) of
        true ->
            { lists:delete(X, Allocated), [X | Free] };
        false ->
            State
    end.
