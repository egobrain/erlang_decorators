-module(log).
-compile([{parse_transform,decorators}]).

-export([log/4]).
-export([sum/2,
	 fact/1]).

-define(LOG,-decorate({?MODULE,log,[?MODULE],verbose})).

% Decorator
log(F,Args,{FunName,_Line},_Module)->
    Level = case get('$level') of
                undefined -> 0;
                Int when Int < 0 -> 0;
                Int -> Int
            end,
    Sp = "  ",
    Spacer = [Sp || _ <- lists:seq(1,Level)],
    ArgsStrList = [io_lib:format("~p",[E]) || E<-Args],
    ArgsStr = string:join(ArgsStrList,","),
    io:format("~s~p(~s) {~n",[Spacer,FunName,ArgsStr]),
    put('$level',Level+1),
    R=apply(F,[Args]),
    put('$level',Level),
    
    io:format("~s~p~n",[[Sp|Spacer],R]),
    io:format("~s}~n",[Spacer]),
    R.

?LOG.
sum(A,B) -> A+B.
    
?LOG.
fact(N) when is_integer(N) andalso N>=1  ->
    fact(N,1).

% Be carefull with decorators and recousive functions.
% Decorator can make tail recursive function non-tail recursive
?LOG.
fact(1,Acc) -> Acc;
fact(N,Acc) -> fact(N-1,Acc*N).
