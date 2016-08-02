-module(postfix_parser).
-export([parseExpression/1, runTestExpressions/0]).

tokeniseExpression(ExpStr) ->
    % Returns a list of tokens
    string:tokens(ExpStr, " ").

evaluateExpression([], Eval) ->
    [Result] = Eval,
    Result;
evaluateExpression(Exp, Eval) ->
    % Get head of list 
    [H|T] = Exp,
    case H of 
        "0" -> evaluateExpression(T, [false | Eval]);
        "1" -> evaluateExpression(T, [true | Eval]);
        Operator ->
            Result = applyOperator(Operator, Eval),
            evaluateExpression(T, Result)
    end.


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
