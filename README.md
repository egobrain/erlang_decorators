erlang_decorators
=================

This code implenets decorators for erlang.

Usage
=====

```Erlang

-module(my_module).

-compile([{parse_transform, decorators}]).

%% decorator
-export([
         log_result/2
        ]).

%% Decorated function
-export([
         foo/1
        ]).

log_result(Fun, Args) ->
    Result = Fun(Args),
    io:format("Result is: ~p", [Result]),
    Result.

-decorate({?MODULE, log_result}).
foo(Bar) ->
    Bar*100.

```