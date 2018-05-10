[![Build Status](https://travis-ci.org/egobrain/decimal.png?branch=master)](https://travis-ci.org/egobrain/erlang_decorators.png?branch=master)
[![GitHub tag](https://img.shields.io/github/tag/egobrain/erlang_decorators.svg)](https://github.com/egobrain/erlang_decorators)
[![Hex.pm](https://img.shields.io/hexpm/v/erlang_decorators.svg)](https://hex.pm/packages/erlang_decorators)

erlang_decorators
=================

This code implenets decorators for erlang.

Usage
=====

Internal
------

First of all you need set `{parser_transform, decorators}` compile option.

```Erlang
-compile([{parse_transform, decorators}]).
```
and then specify `decorate` attribute before funcion definition.

```Erlang
-decorate(DecoratorSpec).
my_function(...) -> ... .
```

DecoratorSpec:
   - `{Module :: module(), Fun :: atom()}` - will call `Module:Fun(F, Args)`, where
`F` is function with arity 1, which takes a list as argumets and apply this list to original function,
and `Args` is a decorated function arguments list.
   - `{Module :: module(), Fun :: atom(), Args :: [any()]}` - same as previous, but you can
specify aditional decorator function arguments. `Module:Fun(F, Args, Arg1, Arg2, ...) will be called.
   - `{Module :: module(), Fun :: atom(), Args :: [any()], verbose}` - if you specify `verbose` option,
the third decorator function argument will be an a tuple of original function name and code line:
`{FuncName :: atom(), Line :: non_neg_integer()}`. `Module:Fun(F, Args, {FunName, Line}, Arg1, Arg2, ...) will be called.

External
--------

You can also specify decorators from outside of module at compile time.

First of all you must tell compiler to use *decorators* parse_transform globaly.
And then specify compile `decorate` option, which describes how to decorate modules functions.
Syntax:
```Erlang
{decorate,
    [
     {Module :: module(),
     [
      {{Fun :: atom(), Arity :: non_neg_integer() | '*'} | '*',
       [DecoratorSpec, ...]},
      ...
     ],
     ...
    ]}
```

Examples
========

Internal
--------

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


External
--------

*rebar.config*

```Erlang
{erl_opts,
 [
  {parse_transform, decorators},
  {decorate,
   [
    {my_module1,
     [
      {{fun1, 0}, [{my_decorators, decorator1}]},
      {{fun2, 0}, [
                   {my_decorators, decorator1},
                   {my_decorators, decorator2}
                  ]},
     ]},
    {my_module2,
     [
      {{fun2, 0}, [
                   {my_decorators, decorator3, [tag]},
                   {my_decorators, decorator4, [tag], verbose}
                  ]}
     ]}
   ]},
   ...
 ]}.

```
