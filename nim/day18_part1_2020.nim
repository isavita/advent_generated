import os
import strutils

proc evaluateExpression(expression: string): int =
  var result = 0
  var currentOp = '+'
  var i = 0
  while i < expression.len:
    let c = expression[i]
    if c == ' ':
      inc i
      continue
    if c == '(':
      var j = i
      var count = 0
      while j < expression.len:
        if expression[j] == '(':
          inc count
        elif expression[j] == ')':
          dec count
        if count == 0:
          break
        inc j
      let subExpr = expression[(i+1)..<j]
      let subResult = evaluateExpression(subExpr)
      if currentOp == '+':
        result += subResult
      else:
        result *= subResult
      i = j
    elif c == '+' or c == '*':
      currentOp = c
    else:
      let num = c.int - '0'.int
      if currentOp == '+':
        result += num
      else:
        result *= num
    inc i
  result

let file = open("input.txt")
var total = 0
for line in file.lines:
  total += evaluateExpression(line)
echo total