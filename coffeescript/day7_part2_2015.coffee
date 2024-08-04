fs = require 'fs'

someAssemblyRequired = (input) ->
  wireToRule = {}

  for inst in input.split("\n")
    [rule, wire] = inst.split(" -> ")
    wireToRule[wire] = rule

  aSignal = memoDFS wireToRule, "a", {}
  wireToRule["b"] = aSignal.toString()
  memoDFS wireToRule, "a", {}

memoDFS = (graph, entry, memo) ->
  return memo[entry] if memo[entry]?

  return parseInt(entry) if /[0-9]/.test(entry)

  sourceRule = graph[entry]
  parts = sourceRule.split(" ")

  switch
    when parts.length == 1
      result = memoDFS graph, parts[0], memo
    when parts[0] == "NOT"
      start = memoDFS graph, parts[1], memo
      result = (Math.pow(2, 16) - 1) ^ start
    when parts[1] == "AND"
      result = memoDFS(graph, parts[0], memo) & memoDFS(graph, parts[2], memo)
    when parts[1] == "OR"
      result = memoDFS(graph, parts[0], memo) | memoDFS(graph, parts[2], memo)
    when parts[1] == "LSHIFT"
      result = memoDFS(graph, parts[0], memo) << memoDFS(graph, parts[2], memo)
    when parts[1] == "RSHIFT"
      result = memoDFS(graph, parts[0], memo) >> memoDFS(graph, parts[2], memo)

  memo[entry] = result
  result

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading from file:", err
  else
    console.log someAssemblyRequired data.trim()