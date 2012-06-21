-module(decorators).
-include_lib("eunit/include/eunit.hrl").

-export([parse_transform/2, pretty_print/1]).


% TODO: сообщения об ошибках в декораторе
parse_transform(Ast,_Options)->
    %io:format("~p~n=======~n",[Ast]),
    %io:format("~s~n=======~n",[pretty_print(Ast)]),
    {ExtendedAst2, RogueDecorators} = lists:mapfoldl(fun transform_node/2, [], Ast),
    Ast2 = lists:flatten(lists:filter(fun(Node)-> Node =/= nil end, ExtendedAst2))
	++ emit_errors_for_rogue_decorators(RogueDecorators),
    %io:format("~p~n<<<<~n",[Ast2]),
    io:format("~s~n>>>>~n",[pretty_print(Ast2)]),
    Ast2.


pretty_print(Ast) -> lists:flatten([erl_pp:form(N) || N<-Ast]).

emit_errors_for_rogue_decorators(DecoratorList)->
    [{error,{Line,erl_parse,["rogue decorator ", io_lib:format("~p",[D]) ]}} || {attribute, Line, decorate, D} <- DecoratorList].

% Трансформатор нод
% описано где-то тут http://www.erlang.org/doc/apps/erts/absform.html
% на выходе nil (чтобы убрать ноду), нода или список нод.
transform_node(Node={attribute, _Line, decorate, _Decorator}, DecoratorList) ->
    % Аккумулируем все декораторы одной функции
    {nil, [Node|DecoratorList]};
transform_node(Node={function, _Line, _FuncName, _Arity, _Clauses}, []) ->
    % Пропускаем функцию без декораторов
    {Node, []};
transform_node(Node={function, _Line, _FuncName, _Arity, _Clauses}, DecoratorList) ->
    % Декорируем
    {apply_decorators(Node,DecoratorList), []};
transform_node(Node={eof,_Line}, DecoratorList) ->
    {[Node| emit_errors_for_rogue_decorators(DecoratorList) ], []};
transform_node(Node, DecoratorList) ->
    % Все остальное
    {Node, DecoratorList}.




apply_decorators(Node={function, Line, FuncName, Arity, _Clauses}, DecoratorList) when length(DecoratorList) > 0 ->
    A = [
     % Оригинальная, переименованная функция
     function_form_original(Node),
     % Замена оригинальной функции на нашу цепочку декораторов
     % 
     function_form_trampoline(Line, FuncName, Arity, DecoratorList),
     % Функция funname_arityn_0 для преобразования входных параметров в единый список
     function_form_unpacker(Line,FuncName,Arity)
     % Цепочка декораторов
     | function_forms_decorator_chain(Line, FuncName, Arity, DecoratorList)
    ],A.


function_form_original({function, Line, FuncName, Arity, Clauses}) ->
    {function, Line, generated_func_name({original,FuncName}), Arity, Clauses}.


% возвращает замену оригинальной функции, переадресовывая вызов на цепь декораторов,
% заменяя входные аргументы на их список
function_form_trampoline(Line, FuncName, Arity, DecoratorList) ->
    NumDecorators = length(DecoratorList),
    ArgNames = arg_names(Arity),
    { function, Line, FuncName, Arity,
      [{clause,
	Line,
	emit_arguments(Line, ArgNames),
	emit_guards(Line, []),
	[
	 emit_local_call( Line,
			  generated_func_name({decorator_wrapper, FuncName, Arity, NumDecorators}),
			  [emit_atom_list(Line, ArgNames)]
			 )
	]
       }]
     }.

% Функция обратная предыдущей, на вход получает список аргументов и вызывает оригинальную функцию  
function_form_unpacker(Line,FuncName,Arity) ->
    ArgNames = arg_names(Arity),
    OriginalFunc = generated_func_name({original,FuncName}),
    {function, Line,
     generated_func_name({decorator_wrapper, FuncName, Arity, 0}),
     1,
     [{clause,
       Line,
       [emit_atom_list(Line, ArgNames)],
       emit_guards(Line, []),
       [{call,
	 Line,
	 {atom,Line,OriginalFunc},
	 emit_arguments(Line,ArgNames)}
       ]}
     ]}.

function_forms_decorator_chain(Line, FuncName, Arity, DecoratorList) ->
    NumDecorators = length(DecoratorList),
    DecoratorIndexes = lists:zip(DecoratorList, lists:seq(1, NumDecorators)),
    [ function_form_decorator_chain(Line,FuncName,Arity,D,I)
      || { {attribute,_,decorate,D},I} <- DecoratorIndexes ] .


function_form_decorator_chain(Line,FuncName,Arity, DecOptions, DecoratorIndex) ->
    NextFuncName = generated_func_name({decorator_wrapper, FuncName, Arity, DecoratorIndex-1}),
    {function, Line,
     generated_func_name({decorator_wrapper, FuncName,Arity, DecoratorIndex}), % name
     1, % arity
     [{ clause, Line,
	emit_arguments(Line, ['ArgList'] ),
	emit_guards(Line, []),
	[
	 % F = DecMod:DecFun( fun NextFun/1, ArgList),
	 emit_decorated_fun(Line, 'F', DecOptions, NextFuncName, 'ArgList',FuncName)
	 % call 'F'
	 % {call, Line,{var,Line,'F'},[]}
	]
       }]
    }.

emit_decorated_fun(Line, _Name, DecMod, DecFun, InnerFunName, ArgName,EArgs)->
%    {match,Line,
%     {var,Line,Name},
     {call,Line,
      {remote, Line, {atom,Line,DecMod},{atom,Line,DecFun}},
      [
       {'fun',Line,{function, InnerFunName, 1}},
       {var, Line, ArgName}
      ]++EArgs
 %    }
    }.

emit_decorated_fun(Line, Name, {DecMod, DecFun}, InnerFunName, ArgName,_OriginalFuncName)->
    emit_decorated_fun(Line, Name, DecMod, DecFun, InnerFunName, ArgName,[]);
emit_decorated_fun(Line, Name, {DecMod, DecFun,verbose}, InnerFunName, ArgName,OriginalFuncName)->
    emit_decorated_fun(Line, Name, DecMod, DecFun, InnerFunName, ArgName,[{atom, Line, {OriginalFuncName,Line}}]);
emit_decorated_fun(Line, Name, {DecMod, DecFun,EArgs}, InnerFunName, ArgName,_OriginalFuncName) when is_list(EArgs)->
    emit_decorated_fun(Line, Name, DecMod, DecFun, InnerFunName, ArgName,emit_values(Line,EArgs));
emit_decorated_fun(Line, Name, {DecMod, DecFun, EArgs,verbose}, InnerFunName, ArgName,OriginalFuncName) when is_list(EArgs) ->
    emit_decorated_fun(Line, Name, DecMod, DecFun, InnerFunName, ArgName,[{atom, Line, {OriginalFuncName,Line}}]++emit_values(Line,EArgs)).

emit_local_call(Line, FuncName, ArgList) ->
    {call, Line, {atom, Line, FuncName}, ArgList}.

emit_arguments(Line, AtomList) ->
    [{var,Line,Arg} || Arg <- AtomList].

emit_values(Line,Args) ->
    [{atom,Line,Arg} || Arg <- Args].    

emit_guards(_Line, [])->
    [];
emit_guards(_,_)->
    throw(nyi).

emit_atom_list(Line, AtomList) ->
    % build a list of args out of cons cells
    % {cons,43,{var,43,'Arg1'},{cons,43,{var,43,'Arg2'},{nil,43}}}
    lists:foldr(fun(Arg, Acc) ->
			{cons, Line, {var, Line, Arg}, Acc}
		end, {nil,Line}, AtomList).

generated_func_name( {original, OrigName} ) ->
    atom_name([OrigName, "_original___"]);
generated_func_name( {trampoline, OrigName} ) ->
    OrigName;
generated_func_name( {decorator_wrapper, OrigName, Arity, N} ) ->
    atom_name([OrigName, "_arity", Arity, "_", N]).

% list() -> atom()
atom_name(Elements) ->
    list_to_atom(lists:flatten(lists:map(
				 fun
				     (A) when is_atom(A) -> atom_to_list(A);
				     (A) when is_number(A) -> io_lib:format("~p",[A]);
				     (A) when is_binary(A) -> io_lib:format("~s",[A]);
				     (A) when is_list(A) -> io_lib:format("~s",[A])
				 end,
				 Elements
				))).
arg_names(Arity) ->
    [ atom_name(["Arg", ArgNum]) || ArgNum <- lists:seq(1,Arity)].
