lpeg = require "lpeg"

testExpressions = {
  "0 1 R",
  "0 0 R",
  "1 0 A 1 R N N",
  "0 0 A 0 N 0 N A R",
  "0 1 A 0 N 1 N A R",
  "1 0 A 1 N 0 N A R",
  "1 1 A 1 N 1 N A R",
  "1 1 A 1 N 1 N A X",
  "1 1 A 0 N 0 N A X"
}

-- The set of whitespace characters. 
space = lpeg.S(" \t\n\r")

--[[
  Capturing operands
  - Define pattern for operand
  - Combine it with arbitrary spaces
  - Then capture operands as table and print them out.
  - For the input, I get {0,1} as expected. 
]]
operand = lpeg.R("01")^1 / tostring
operand_or_space = operand + space

extract_ops = lpeg.Ct(operand_or_space^0)
optable = extract_ops:match("0    1 2")
if optable and #optable > 0  then
  print("1: " .. table.concat(optable, ","))
end

--[[
  What do I want to do now?
  - Identify when an operation appears
]]

-- The set of operators (just the 2-arity ones for now)
operator = lpeg.S("AO")^1 / tostring
operator_or_space = space + operator
print("2: " .. (operator:match("1 A") or ""))

operand_operator = operand_or_space^1 * operator^1
print("3: " .. operand_operator:match("0 1 A"))

-- FFS, this is hard to reason about.
-- Which probably means I'm reasoning about it incorrectly.....
-- Works, but I don't know why. 
extract = lpeg.Ct(operand_operator)
extractTable = extract:match("0 1 A")
if extractTable and #extractTable > 0 then
  print("4: " .. table.concat(extractTable, ","))
end





