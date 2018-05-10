-module(ext_dec_test).

-export([
         w1/0, w2/0,
         t1/0, t2/0,
         i1/0, i2/0,
         it1/0, it2/0
        ]).

%% =============================================================================
%%% Functions
%% =============================================================================

w1() -> 1.

w2() -> 2.

t1() -> 1.

t2() -> 2.

i1() -> 1.

i2() -> 2.

it1() -> 1.

it2() -> 2.

%% =============================================================================
%%% Tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

w1_test() ->
    ?assertEqual({wrap, 1}, w1()).

w2_test() ->
    ?assertEqual({wrap, {wrap, 2}}, w2()).

t1_test() ->
    ?assertEqual({tag1, 1}, t1()).

t2_test() ->
    ?assertEqual({tag1, {tag2, 2}}, t2()).

i1_test() ->
    ?assertEqual({i1, 1}, i1()).

i2_test() ->
    ?assertEqual({i2, {i2, 2}}, i2()).

it1_test() ->
    ?assertEqual({{tag1, it1}, 1}, it1()).

it2_test() ->
    ?assertEqual({{tag1, it2}, {{tag2, it2}, {{tagW, it2}, 2}}}, it2()).

-endif.
