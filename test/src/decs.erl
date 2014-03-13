-module(decs).

-export([
         wrap/2,
         tag/3,
         info/3,
         info_tag/4
        ]).

wrap(F, Args) ->
    {wrap, F(Args)}.

tag(F, Args, Tag) ->
    {Tag, F(Args)}.

info(F, Args, {FunName, _Line}) ->
    {FunName, F(Args)}.

info_tag(F, Args, {FunName, _Line}, Tag) ->
    {{Tag, FunName}, F(Args)}.
