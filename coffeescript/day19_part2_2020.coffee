fs = require 'fs'

solve = (input) ->
  [graph, messages] = parseInput(input)
  
  fillInGraph graph, '42'
  fillInGraph graph, '31'
  
  part42 = "(#{graph['42'].resolved.join '|'})"
  part31 = "(#{graph['31'].resolved.join '|'})"
  
  rule8String = "(#{part42})+"
  
  makeRegexp = (num) ->
    new RegExp "^#{rule8String}#{part42}{#{num}}#{part31}{#{num}}$"
  
  matchRuleZero = 0
  for m in messages
    for i in [1..10]
      pattern = makeRegexp i
      if pattern.test m
        matchRuleZero++
        break
  
  matchRuleZero

fillInGraph = (graph, entry) ->
  return if not graph[entry]
  return graph[entry].resolved if graph[entry].resolved?.length > 0
  
  graph[entry].resolved = []
  for option in graph[entry].options
    resolved = ['']
    for entryPoint in option
      nestedResolveVals = fillInGraph graph, entryPoint
      newResolved = []
      for nextPiece in nestedResolveVals
        for prev in resolved
          newResolved.push prev + nextPiece
      resolved = newResolved
    graph[entry].resolved.push resolved...
  
  graph[entry].resolved

parseInput = (input) ->
  parts = input.split '\n\n'
  
  rules = {}
  for r in parts[0].split '\n'
    if /[a-z]/.test r
      [num, char] = /^(\d+): "(\w)"$/.exec(r).slice(1)
      rules[num] = {resolved: [char]}
    else
      [key, ruleParts] = r.split ': '
      newRule = {options: []}
      for ruleNums in ruleParts.split ' | '
        option = (ruleNums.split ' ').map (n) -> n.trim()
        newRule.options.push option
      rules[key] = newRule
  
  messages = parts[1].split '\n'
  
  [rules, messages]

toInt = (s) ->
  parseInt s, 10

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  input = data.trim()
  result = solve input
  console.log result