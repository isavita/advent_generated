
import tables, sequtils, strutils, re, os

type
  Rule = object
    resolved: seq[string]
    options: seq[seq[int]]

proc fillInGraph(graph: var Table[int, Rule], entry: int): seq[string] =
  if graph[entry].resolved.len > 0:
    return graph[entry].resolved

  var currentResolved: seq[string]
  
  for option in graph[entry].options:
    var resolvedForOption: seq[string] = @[""]
    
    for entryPoint in option:
      let nestedResolveVals = fillInGraph(graph, entryPoint)
      var newResolvedForOption: seq[string]
      
      for nextPiece in nestedResolveVals:
        for prev in resolvedForOption:
          newResolvedForOption.add(prev & nextPiece)
          
      resolvedForOption = newResolvedForOption
      
    currentResolved.add(resolvedForOption)
    
  graph[entry].resolved = currentResolved
  return graph[entry].resolved

proc parseInput(inputData: string): (Table[int, Rule], seq[string]) =
  let parts = inputData.split("\n\n")

  var rules: Table[int, Rule]

  for line in parts[0].splitLines():
    if line.len == 0: continue

    let partsColon = line.split(':')
    let key = partsColon[0].parseInt()
    let ruleBody = partsColon[1].strip()

    if ruleBody[0] == '"':
      var R: Rule
      R.resolved = @[ruleBody[1 .. ^2]]
      rules[key] = R
    else:
      var R: Rule
      R.options = newSeq[seq[int]]()
      
      for optionStr in ruleBody.split(" | "):
        let nums = optionStr.split().mapIt(it.parseInt())
        R.options.add(nums)
      
      rules[key] = R

  var messages: seq[string]
  for line in parts[1].splitLines():
    if line.len > 0:
      messages.add(line)

  (rules, messages)

proc solve(): int =
  let inputData = readFile("input.txt")
  var (graph, messages) = parseInput(inputData)

  let R42 = fillInGraph(graph, 42)
  let R31 = fillInGraph(graph, 31)

  let part42Str = "(" & R42.join("|") & ")"
  let part31Str = "(" & R31.join("|") & ")"

  let rule8String = "(" & part42Str & ")+"

  var compiledPatterns: seq[Regex]
  
  for i in 1..9:
    var patternStr = "^" & rule8String
    
    for _ in 0..<i:
      patternStr &= part42Str
      
    for _ in 0..<i:
      patternStr &= part31Str
      
    patternStr &= "$"
    
    compiledPatterns.add(re(patternStr))
    
  var matchRuleZero = 0
  
  for m in messages:
    for pattern in compiledPatterns:
      if m.match(pattern):
        inc matchRuleZero
        break
        
  result = matchRuleZero

when isMainModule:
  echo solve()
