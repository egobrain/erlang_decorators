-module(dec_test).

-compile([{parse_transform, decorators}]).

-export([
         wrap/2,
         tag/3,
         info/3,
         info_tag/4
        ]).

-export([
         w1/0, w2/0,
         t1/0, t2/0,
         i1/0, i2/0,
         it1/0, it2/0
        ]).

%% =============================================================================
%%% Decorators
%% =============================================================================

wrap(F, Args) ->
    {wrap, F(Args)}.

tag(F, Args, Tag) ->
    {Tag, F(Args)}.

info(F, Args, {FunName, _Line}) ->
    {FunName, F(Args)}.

info_tag(F, Args, {FunName, _Line}, Tag) ->
    {{Tag, FunName}, F(Args)}.

%% =============================================================================
%%% Functions
%% =============================================================================

-decorate({?MODULE, wrap}).
w1() -> 1.

-decorate({?MODULE, wrap}).
-decorate({?MODULE, wrap}).
w2() -> 2.

-decorate({?MODULE, tag, [tag1]}).
t1() -> 1.

-decorate({?MODULE, tag, [tag1]}).
-decorate({?MODULE, tag, [tag2]}).
t2() -> 2.

-decorate({?MODULE, info, [], verbose}).
i1() -> 1.

-decorate({?MODULE, info, [], verbose}).
-decorate({?MODULE, info, [], verbose}).
i2() -> 2.

-decorate({?MODULE, info_tag, [tag1], verbose}).
it1() -> 1.

-decorate({?MODULE, info_tag, [tag1], verbose}).
-decorate({?MODULE, info_tag, [tag2], verbose}).
it2() -> 2.

%% =============================================================================
%%% Tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

w1_test() ->
    ?assertEqual(w1(), {wrap, 1}).

w2_test() ->
    ?assertEqual(w2(), {wrap, {wrap, 2}}).

t1_test() ->
    ?assertEqual(t1(), {tag1, 1}).

t2_test() ->
    ?assertEqual(t2(), {tag1, {tag2, 2}}).

i1_test() ->
    ?assertEqual(i1(), {i1, 1}).

i2_test() ->
    ?assertEqual(i2(), {i2, {i2, 2}}).

it1_test() ->
    ?assertEqual(it1(), {{tag1, it1}, 1}).

it2_test() ->
    ?assertEqual(it2(), {{tag1, it2}, {{tag2, it2}, 2}}).

-endif.
