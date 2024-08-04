fs = require 'fs'

someAssemblyRequired = (input) ->
  wireToRule = {}
  for inst in input.split('\n')
    [rule, wire] = inst.split(' -> ')
    wireToRule[wire] = rule

  memoDFS wireToRule, 'a', {}

memoDFS = (graph, entry, memo) ->
  return memo[entry] if memo[entry]?

  return parseInt(entry) if /[0-9]/.test(entry)

  sourceRule = graph[entry]
  parts = sourceRule.split(' ')

  switch
    when parts.length is 1
      result = memoDFS graph, parts[0], memo
    when parts[0] is 'NOT'
      start = memoDFS graph, parts[1], memo
      result = (1 << 16) - 1 ^ start
    when parts[1] is 'AND'
      result = memoDFS(graph, parts[0], memo) & memoDFS(graph, parts[2], memo)
    when parts[1] is 'OR'
      result = memoDFS(graph, parts[0], memo) | memoDFS(graph, parts[2], memo)
    when parts[1] is 'LSHIFT'
      result = memoDFS(graph, parts[0], memo) << memoDFS(graph, parts[2], memo)
    when parts[1] is 'RSHIFT'
      result = memoDFS(graph, parts[0], memo) >> memoDFS(graph, parts[2], memo)

  memo[entry] = result
  result

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.error 'Error reading from file:', err
    return

  input = data.trim()
  console.log someAssemblyRequired(input)