-module(decorators).
-include_lib("eunit/include/eunit.hrl").

-export([parse_transform/2, pretty_print/1]).

-record(state, {
          opt,
          decorators=[]
         }).

parse_transform(Ast, Options)->
    DecorateOpt =
        case lists:keyfind(decorate, 1, Options) of
            {decorate, DecorateList} ->
                DecorateList;
            false ->
                []
        end,
    %% io:format("~p~n=======~n", [Ast]),
    %% io:format("~s~n=======~n", [pretty_print(Ast)]),
    {ExtendedAst2, RogueDecorators, _State} =
        mapfoldl2(fun transform_node/3, [], #state{opt=DecorateOpt}, Ast),
    Ast2 = lists:flatten(lists:filter(fun(Node)-> Node =/= nil end, ExtendedAst2))
        ++ emit_errors_for_rogue_decorators(RogueDecorators),
    %% io:format("~p~n<<<<~n", [Ast2]),
    %% io:format("~s~n>>>>~n", [pretty_print(Ast2)]),
    Ast2.

pretty_print(Ast) -> lists:flatten([erl_pp:form(N) || N<-Ast]).

emit_errors_for_rogue_decorators(DecoratorList)->
    [{error, {Line, erl_parse, ["rogue decorator ", io_lib:format("~p", [D]) ]}}
     || {attribute, Line, decorate, D} <- DecoratorList].

transform_node(Node={attribute, _Line, module, Module}, DecoratorList, #state{opt=DecorateOpt} = State) ->
    State2 =
        case lists:keyfind(Module, 1, DecorateOpt) of
            {_, Decorators} ->
                State#state{decorators = Decorators};
            false ->
                State#state{decorators = []}
        end,
    {Node, DecoratorList, State2};
transform_node(_Node={attribute, _Line, decorate, Decorator}, DecoratorList, State) ->
    {nil, [Decorator | DecoratorList], State};
transform_node(Node={function, _Line, FuncName, Arity, _Clauses}, DecoratorList,
               #state{decorators=Decorators} = State) ->
    DecoratorList2 = DecoratorList ++ get_ext_decorators(FuncName, Arity, Decorators),
    {apply_decorators(Node, DecoratorList2), [], State};
transform_node(Node={eof, _Line}, DecoratorList, State) ->
    {[Node | emit_errors_for_rogue_decorators(DecoratorList) ], [], State};
transform_node(Node, DecoratorList, State) ->
    {Node, DecoratorList, State}.

get_ext_decorators(FuncName, Arity, DecoratorList) ->
    get_ext_decorators_(FuncName, Arity, DecoratorList, []).

get_ext_decorators_(_FuncName, _Arity, [], Acc) ->
    lists:append(Acc);
get_ext_decorators_(FuncName, Arity, [{{FuncName, Arity}, Decorators}|Rest], Acc) ->
    get_ext_decorators_(FuncName, Arity, Rest, [lists:reverse(Decorators)|Acc]);
get_ext_decorators_(FuncName, Arity, [{{FuncName, '*'}, Decorators}|Rest], Acc) ->
    get_ext_decorators_(FuncName, Arity, Rest, [lists:reverse(Decorators)|Acc]);
get_ext_decorators_(FuncName, Arity, [{'*', Decorators}|Rest], Acc) ->
    get_ext_decorators_(FuncName, Arity, Rest, [lists:reverse(Decorators)|Acc]);
get_ext_decorators_(FuncName, Arity, [_|Rest], Acc) ->
    get_ext_decorators_(FuncName, Arity, Rest, Acc).

apply_decorators(Node={function, _Line, _FuncName, _Arity, _Clauses}, []) ->
    Node;
apply_decorators(Node={function, Line, FuncName, Arity, _Clauses}, DecoratorList) ->
    [
     function_form_original(Node),
     function_form_trampoline(Line, FuncName, Arity, DecoratorList),
     function_form_unpacker(Line, FuncName, Arity)
     | function_forms_decorator_chain(Line, FuncName, Arity, DecoratorList)
    ].

function_form_original({function, Line, FuncName, Arity, Clauses}) ->
    {function, Line, generated_func_name({original, FuncName}), Arity, Clauses}.

function_form_trampoline(Line, FuncName, Arity, DecoratorList) ->
    NumDecorators = length(DecoratorList),
    ArgNames = arg_names(Arity),
    { function, Line, FuncName, Arity,
      [{clause,
        Line,
        emit_arguments(Line, ArgNames),
        emit_guards(Line, []),
        [
         emit_local_call(
           Line,
           generated_func_name({decorator_wrapper, FuncName, Arity, NumDecorators}),
           [emit_atom_list(Line, ArgNames)])
        ]
       }]
    }.

function_form_unpacker(Line, FuncName, Arity) ->
    ArgNames = arg_names(Arity),
    OriginalFunc = generated_func_name({original, FuncName}),
    {function, Line,
     generated_func_name({decorator_wrapper, FuncName, Arity, 0}),
     1,
     [{clause,
       Line,
       [emit_atom_list(Line, ArgNames)],
       emit_guards(Line, []),
       [{call,
         Line,
         {atom, Line, OriginalFunc},
         emit_arguments(Line, ArgNames)}
       ]}
     ]}.

function_forms_decorator_chain(Line, FuncName, Arity, DecoratorList) ->
    NumDecorators = length(DecoratorList),
    DecoratorIndexes = lists:zip(DecoratorList, lists:seq(1, NumDecorators)),
    [ function_form_decorator_chain(Line, FuncName, Arity, D, I)
      || {D, I} <- DecoratorIndexes ].


function_form_decorator_chain(Line, FuncName, Arity, DecOptions, DecoratorIndex) ->
    NextFuncName = generated_func_name({decorator_wrapper, FuncName, Arity, DecoratorIndex-1}),
    {function, Line,
     generated_func_name({decorator_wrapper, FuncName, Arity, DecoratorIndex}), %% name
     1,
     [{ clause, Line,
        emit_arguments(Line, ['ArgList'] ),
        emit_guards(Line, []),
        [
         emit_decorated_fun(Line, 'F', DecOptions, NextFuncName, 'ArgList', FuncName)
        ]
      }]
    }.

emit_decorated_fun(Line, _Name, DecMod, DecFun, InnerFunName, ArgName, EArgs) ->
    {call, Line,
     {remote, Line, {atom, Line, DecMod}, {atom, Line, DecFun}},
     [
      {'fun', Line, {function, InnerFunName, 1}},
      {var, Line, ArgName}
     ] ++ EArgs
    }.

emit_decorated_fun(Line, Name, {DecMod, DecFun}, InnerFunName, ArgName, _OriginalFuncName)->
    emit_decorated_fun(Line, Name, DecMod, DecFun, InnerFunName, ArgName, []);
emit_decorated_fun(Line, Name, {DecMod, DecFun, verbose}, InnerFunName, ArgName, OriginalFuncName)->
    emit_decorated_fun(Line, Name, DecMod, DecFun, InnerFunName, ArgName, [{atom, Line, {OriginalFuncName, Line}}]);
emit_decorated_fun(Line, Name, {DecMod, DecFun, EArgs}, InnerFunName, ArgName, _OriginalFuncName) when is_list(EArgs)->
    emit_decorated_fun(Line, Name, DecMod, DecFun, InnerFunName, ArgName, emit_values(Line, EArgs));
emit_decorated_fun(Line, Name, {DecMod, DecFun, EArgs, verbose}, InnerFunName, ArgName, OriginalFuncName) when is_list(EArgs) ->
    emit_decorated_fun(Line, Name, DecMod, DecFun, InnerFunName, ArgName, [{atom, Line, {OriginalFuncName, Line}}]++emit_values(Line, EArgs)).

emit_local_call(Line, FuncName, ArgList) ->
    {call, Line, {atom, Line, FuncName}, ArgList}.

emit_arguments(Line, AtomList) ->
    [{var, Line, Arg} || Arg <- AtomList].

emit_values(Line, Args) ->
    [{atom, Line, Arg} || Arg <- Args].

emit_guards(_Line, [])->
    [];
emit_guards(_, _)->
    throw(nyi).

emit_atom_list(Line, AtomList) ->
    lists:foldr(fun(Arg, Acc) ->
                        {cons, Line, {var, Line, Arg}, Acc}
                end, {nil, Line}, AtomList).

generated_func_name( {original, OrigName} ) ->
    atom_name([OrigName, "_original___"]);
generated_func_name( {trampoline, OrigName} ) ->
    OrigName;
generated_func_name( {decorator_wrapper, OrigName, Arity, N} ) ->
    atom_name([OrigName, "_arity", Arity, "_", N]).

atom_name(Elements) ->
    list_to_atom(
      lists:flatten(
        lists:map(
          fun
              (A) when is_atom(A) -> atom_to_list(A);
              (A) when is_number(A) -> io_lib:format("~p", [A]);
              (A) when is_binary(A) -> io_lib:format("~s", [A]);
              (A) when is_list(A) -> io_lib:format("~s", [A])
          end,
          Elements))).
arg_names(Arity) ->
    [ atom_name(["Arg", ArgNum]) || ArgNum <- lists:seq(1, Arity)].

mapfoldl2(Fun, State1, State2, List) ->
    mapfoldl2(Fun, State1, State2, List, []).

mapfoldl2(_Fun, State1, State2, [], Acc) ->
    {lists:reverse(Acc), State1, State2};
mapfoldl2(Fun, State1, State2, [H|T], Acc) ->
    {R, S1, S2} = Fun(H, State1, State2),
    mapfoldl2(Fun, S1, S2, T, [R|Acc]).
