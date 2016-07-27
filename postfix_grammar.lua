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
  - Then capture operands as table
]]
operand = lpeg.R("01")^1 / tostring
operand_or_space = operand + space
print(operand_or_space)

extract_ops = lpeg.Ct(operand_or_space^0)
optable = extract_ops:match("0    1 2")
if optable and #optable > 0  then
  print(table.concat(optable, ","))
end
















