-module(postfix_parser).
-export([parseExpression/1, runTestExpressions/0]).

tokeniseExpression(ExpStr) ->
    string:tokens(ExpStr, " ").

evaluateExpression([], Eval) ->
    [Result] = Eval,
    Result;
evaluateExpression(["0"|T], Eval) ->
    evaluateExpression(T, [false | Eval]);
evaluateExpression(["1"|T], Eval) ->
    evaluateExpression(T, [true | Eval]);
evaluateExpression([Operator|T], Eval) ->
    Result = applyOperator(Operator, Eval),
    evaluateExpression(T, Result).

applyOperator(Operator, Eval) ->
    {Arity, OperatorFun} = operatorInfo(Operator),
    {Args, Rest} = lists:split(Arity,Eval),
    [erlang:apply(OperatorFun, Args)|Rest].

operatorInfo(Operator) ->
    % For better reuse, the operator functions could be placed in a separate module
    % and use Erlang's module introspection to match desired operator with its implementation.
    OpMap = #{
        'A' => {2, fun(A,B) -> A and B end},
        'R' => {2, fun(A,B) -> A or B end},
        'X' => {2, fun(A,B) -> A xor B end},
        'N' => {1, fun(A) -> not A end}
      },
    maps:get(list_to_atom(Operator), OpMap).


parseExpression(ExpStr) ->
    evaluateExpression(tokeniseExpression(ExpStr), []).


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
    ParseAndPrint = fun(Expression, Expected) ->
       Result = parseExpression(Expression),
       io:format("~s = ~p. Expected: ~p\n", [Expression, Result, Expected]),
       Result
    end,
    [ParseAndPrint(Expression, Expected) || {Expression, Expected} <- Tests],
    ok. 
