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


  Code Structure:
  - Hmmmm. Need to read in file containing the expressions.
  - Iterate over the lines.
  - Tokenise each line, populating an array for each line. 
  - But each entry into the array needs to be identified as an operator or operand.
  - Simple solution. If it's a 0 or 1 then it's an operand. 
  - For operators, I need to know their arity and effect (but not till evaluation time)
]]


function loadFileReturnFirstLine(filename)
  local firstLine
  for line in io.lines(filename) do
    firstLine = line
    break
  end
  return firstLine
end


function tokeniseExpression(line)
  local tokens = {}
  for word in line:gmatch("%S+") do
    local token = {}
    if (tonumber(word) == 0 or tonumber(word) == 1) then
      token.type = "operand"
    else
      token.type = "operator"
    end
    token.value = word
    table.insert(tokens, token)
  end
  return tokens
end

function popToOperator(expression)
  local popped = {}
  for _, token in ipairs(expression) do
    if token.type == "operand" then
      popped[#popped+1] = token
    end
    if token.type == "operator" then
      popped[#popped+1] = token
      break
    end
  end
  return popped
end

function evaluate(subexp)
	local operator = subexp[#subexp]
	
end

function operatorArity(operator)
  local arity = {
    A = 2,
    R = 2,
    X = 2,
    N = 1
  }
  return arity[operator]
end

function operatorJob(operator)

  local function tobool(num)
    return tonumber(anum)
  end

  local job = {
    A = function(a, b)
      if tobool(a) and tobool(b) then
        return true
      else
        return false
      end
    end,
    R = function(a, b)
      if tobool(a) or tobool(b) then
        return true
      else
       return false 
      end
    end,
    X = function(a, b)
      if tobool(a) and not(tobool(b)) then
        return true
      elseif not(tobool(a)) and tobool(b) then
        return true
      else
        return false
      end
    end,
    N = function(a)
      return not(tobool(a))
    end
  }

  return job[operator]()
end
