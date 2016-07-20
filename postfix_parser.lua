--[[
  First Lua progam in a quite a while.
  Purpose: Parse and evaluate a boolean postfix expression
]]


-- ARRGH GLOBAL
DEBUG = true

function printTable(name, someTable)
  if DEBUG then print(name .. ": " .. table.concat(someTable, ",") .. ".\n") end 
end


--[[
  Functions concerned with loading in the expression information
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
    table.insert(tokens, word)
  end
  return tokens
end


--[[
  The loaded expression is modelled as a stack.
  These functions manipulate the stack to evaluate expressions.
]]

function evaluateExpression(stack)
  printTable("Current stack", stack)
  if #stack == 1 then
    local result = stack[1]
    if result == 0 then
      return false
    else
      return true
    end
  else
    local poppedStack, subexpression = popToOperator(stack)
    local result = evaluateSubexpression(subexpression)
    local newStack = pushToStack(poppedStack, result)
    return evaluateExpression(newStack)
  end
end

function popToOperator(stack)
  local subexp = {}
  local poppedTo = 1
  for stackPosition, token in ipairs(stack) do
    poppedTo = stackPosition
    subexp[#subexp+1] = token
    if tokenType(token) == "operator" then
      break
    end
  end

  local poppedStack = {}
  for i=poppedTo+1,#stack do
    poppedStack[#poppedStack+1] = stack[i]
  end

  printTable("Popped stack", poppedStack)
  printTable("Subexpression", subexp)

  return poppedStack, subexp
end

function evaluateSubexpression(subexp)
	local operator = subexp[#subexp]
  local numArgs = operatorArity(operator)
  local args = {}
  for i=1,numArgs do
    args[i] = subexp[#subexp-i]
  end

  local job = operatorJob(operator)
  local result = job(table.unpack(args))

  -- Return the unused elements of the subexpression
  -- And return the result.
  local resultElements = {}
  for i=1, #subexp-(numArgs+1) do
    resultElements[i] = subexp[i]
  end
  resultElements[#resultElements+1] = result

  printTable("Results of subexpression", resultElements)

  return resultElements
end

function pushToStack(stack, elements)
  -- Cheating by not treating stack as immutable
  for i = #elements,1,-1 do
    table.insert(stack, 1, elements[i])
  end
  return stack
end


--[[
  Functions used to define the behaviour of the various tokens involved in the expressions
]]

--- Determine whether or not the token is an operator or an operand
-- @param token The token being checked
-- @return The type of the token: "operator" or "operand"
function tokenType(token)
  if tonumber(token) == 0 or tonumber(token) == 1 then
    return "operand"
  else
    return "operator"
  end
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

  local function tobool(anum)
    return (tonumber(anum) == 1)
  end

  local job = {
    A = function(a, b)
      if tobool(a) and tobool(b) then
        return 1
      else
        return 0
      end
    end,
    R = function(a, b)
      if tobool(a) or tobool(b) then
        return 1
      else
       return 0
      end
    end,
    X = function(a, b)
      if tobool(a) and not(tobool(b)) then
        return 1
      elseif not(tobool(a)) and tobool(b) then
        return 1
      else
        return 0
      end
    end,
    N = function(a)
      local boolresult = not(tobool(a))
      if boolresult then 
        return 1
      else 
        return  0
      end
    end
  }

  return job[operator]
end


--[[
  Testing, Testing
]]

function testExpressions()
  local test = {
    {"0 1 R",             true},
    {"0 0 R",             false},
    {"1 0 A 1 R N N",     true},
    {"0 0 A 0 N 0 N A R", true},
    {"0 1 A 0 N 1 N A R", false},
    {"1 0 A 1 N 0 N A R", false},
    {"1 1 A 1 N 1 N A R", true},
    {"1 1 A 1 N 1 N A X", true},
    {"1 1 A 0 N 0 N A X", false}
  }
  
  for _, v in ipairs(test) do
    local result = evaluateExpression(tokeniseExpression(v[1]))
    local expectedResult = v[2]
    assert(result == expectedResult)
  end 
end

testExpressions()