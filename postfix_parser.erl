-module(postfix_parser).
-export([parseExpression/1, runTestExpressions/0]).

tokeniseExpression(ExpStr) ->
    % Returns a list of tokens
    string:tokens(ExpStr, " ").

evaluateExpression([], Eval) ->
    [Result] = Eval,
    Result;
evaluateExpression(["0"|T], Eval) ->
    evaluateExpression(T, [false | Eval]);
evaluateExpression(["1"|T], Eval) ->
    evaluateExpression(T, [true | Eval]);
evaluateExpression([Operator|T], Eval) ->
    % This is an operator
    Result = applyOperator(Operator, Eval),
    evaluateExpression(T, Result).

applyOperator(Operator, Eval) ->
    case Operator of 
        "A" ->
            [A,B|T] = Eval,
            Result = A and B;
        "R" ->
            [A,B|T] = Eval,
            Result = A or B;
        "X" ->
            [A,B|T] = Eval,
            Result = A xor B;
        "N" ->
            [A|T] = Eval,
            Result = not A
    end,
    [Result|T].
            

parseExpression(ExpStr) ->
    Result = evaluateExpression(tokeniseExpression(ExpStr), []),
    io:format("~s: ~p\n", [ExpStr, Result]).


runTestExpressions() ->
    Tests = ["0 1 R",
             "0 0 R",
             "1 0 A 1 R N N",
             "0 0 A 0 N 0 N A R",
             "0 1 A 0 N 1 N A R",
             "1 0 A 1 N 0 N A R",
             "1 1 A 1 N 1 N A R",
             "1 1 A 1 N 1 N A X",
             "1 1 A 0 N 0 N A X"
            ],
    [parseExpression(E) || E <- Tests].
