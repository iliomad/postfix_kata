 
function evaluateExpression(expStr)

  local function tokeniseExpression(exp)
    local tokens = {}
    for word in exp:gmatch("%S+") do
      table.insert(tokens, word)
    end
    return tokens
  end

  local stack = makeStack()

  for _, token in ipairs(tokeniseExpression(expStr)) do
    if token == "0" then
      stack.push(false)
    elseif token == "1" then
      stack.push(true)
    else
      -- Assume the token is an operator
      local operator = operatorInfo(token)
      local operands = {}
      for i=1,operator.arity do
        operands[#operands+1] = stack.pop()
      end
      local result = operator.fun(table.unpack(operands))
      stack.push(result)
    end
  end

  return stack.pop()
end

-- Playing with a closure
function makeStack()
  local stack = {}
  local interface = {
    pop = function()
      local val = stack[1]
      table.remove(stack, 1)
      return val
    end,
    push = function(val)
      table.insert(stack, 1, val)
    end
  }
  return interface
end

function operatorInfo(operator)
  local info = {
    A = {arity = 2, fun = function(a, b) return a and b end},
    R = {arity = 2, fun = function(a, b) return a or b end},
    X = {arity = 2, fun = function(a, b) return a ~= b end},
    N = {arity = 1, fun = function(a) return not a end}
  }
  return info[operator]
end

function runTestExpressions()
  local testExpressions = {
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

  for _, v in ipairs(testExpressions) do
    local result = evaluateExpression(v[1])
    local expectedResult = v[2]
    print(v[1] .. " = " .. tostring(result) .. ". Expected: " .. tostring(expectedResult))
  end
end

runTestExpressions()
