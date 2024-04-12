fs = require 'fs'

evaluate = (expression) ->
  tokens = tokenize expression
  evaluateTokens tokens

tokenize = (expression) ->
  expression = expression.replace /\(/g, '( '
  expression = expression.replace /\)/g, ' )'
  expression.split /\s+/

evaluateTokens = (tokens) ->
  ops = []
  vals = []
  for token in tokens
    switch token
      when '(' then ops.push token
      when '+', '*'
        while ops.length > 0 and ops[ops.length - 1] isnt '('
          vals.push applyOp ops.pop(), vals.pop(), vals.pop()
        ops.push token
      when ')'
        while ops[ops.length - 1] isnt '('
          vals.push applyOp ops.pop(), vals.pop(), vals.pop()
        ops.pop() # Remove the opening '('
      else
        vals.push parseInt token, 10

  while ops.length > 0
    vals.push applyOp ops.pop(), vals.pop(), vals.pop()

  vals[0]

applyOp = (op, b, a) ->
  switch op
    when '+' then a + b
    when '*' then a * b
    else throw "Unknown operator: #{op}"

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading file:", err
    return
  sum = 0
  for line in data.trim().split '\n'
    sum += evaluate line
  console.log sum