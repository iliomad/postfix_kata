--[[
  First Lua progam in a quite a while.
  Purpose: Parse and evaluate a boolean postfix expression
  Outline:
  - Read in the line that specifies the expression;
  - Tokenise it;
  - Evaluate it;

  Data structure:
  - Stack;
  - Modelled using array;
  - Tokenised string becomes an array;
  - Top of stack is index 1.
  - Stack contains operands and operators
  - So, I need to know if something is one or the other
  - Assuming correctly formed expressions
  - Pop from stack until operator reached.
  - Then apply the operator to the popped operators
  - Push the output of the operation (and any unused operators) back onto the stack
  - Yuck. There are far cleaner implementations. Could look them up, but that would spoil the fun. 
]]
