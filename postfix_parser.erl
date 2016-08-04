-module(postfix_parser).
-export([parseExpression/1, runTestExpressions/0]).

tokeniseExpression(ExpStr) ->
    % Returns a list of tokens
    % Erlang can't tell the difference between a list of integers and a string.
    string:tokens(ExpStr, " ").

evaluateExpression([], Eval, _) ->
    [Result] = Eval,
    Result;
evaluateExpression(["0"|T], Eval, OpMap) ->
    evaluateExpression(T, [false | Eval], OpMap);
evaluateExpression(["1"|T], Eval, OpMap) ->
    evaluateExpression(T, [true | Eval], OpMap);
evaluateExpression([Operator|T], Eval, OpMap) ->
    % This is an operator
    Result = applyOperator(Operator, Eval, OpMap),
    evaluateExpression(T, Result, OpMap).

applyOperator(Operator, Eval, OpMap) ->
    OperatorFun = maps:get(list_to_atom(Operator), OpMap),
    {_,Arity} = erlang:fun_info(OperatorFun, arity),
    {Args, Rest} = lists:split(Arity,Eval),
    Result = erlang:apply(OperatorFun, Args),
    [Result|Rest].

operatorInfo() ->
     #{
        'A' => fun(A,B) -> A and B end,
        'R' => fun(A,B) -> A or B end,
        'X' => fun(A,B) -> A xor B end,
        'N' => fun(A) -> not A end
      }.


parseExpression(ExpStr) ->
    Result = evaluateExpression(tokeniseExpression(ExpStr), [], operatorInfo()),
    io:format("~s: ~p\n", [ExpStr, Result]),
    Result.


runTestExpressions() ->
    Tests = [{"0 1 R", true},
             {"0 0 R", false},
             {"1 0 A 1 R N N", true},
             {"0 0 A 0 N 0 N A R", true},
             {"0 1 A 0 N 1 N A R", false},
             {"1 0 A 1 N 0 N A R", false},
             {"1 1 A 1 N 1 N A R", true},
             {"1 1 A 1 N 1 N A X", true},
             {"1 1 A 0 N 0 N A X", false}
            ],
    [parseExpression(E) =:= Result || {E, Result} <- Tests].
