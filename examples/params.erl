-module(params).

-compile([{parse_transform,decorators}]).

-export([simple/1,mul_and_add/4]).

-decorate({?MODULE,mul_and_add,[10,3]}).
simple(A) ->
    A.

mul_and_add(F,Args,Arg1,Arg2) ->
    R = F(Args),
    R*Arg1+Arg2.
