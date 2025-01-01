
fs = require 'fs'

parseInput = (input) ->
  workflows = {}
  parts = []
  i = 0
  while input[i] != ""
    [workflowName, rules] = parseWorkflow(input[i])
    workflows[workflowName] = rules
    i++
  i++
  while i < input.length
    parts.push parsePart(input[i])
    i++
  [workflows, parts]

parseWorkflow = (line) ->
  idx = line.indexOf "{"
  workflowName = line[0...idx]
  rules = []
  rulesStr = line[idx + 1...line.length - 1].split ","
  for ruleStr in rulesStr
    rule = {}
    idx = ruleStr.indexOf ":"
    if idx == -1
      rule.workflowName = ruleStr
    else
      rule.category = ruleStr[0]
      rule.operator = ruleStr[1]
      rule.num = parseInt ruleStr[2...idx]
      rule.workflowName = ruleStr[idx + 1...]
    rules.push rule
  [workflowName, rules]

parsePart = (line) ->
  match = line.match /\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}/
  {
    x: parseInt(match[1])
    m: parseInt(match[2])
    a: parseInt(match[3])
    s: parseInt(match[4])
  }

applyWorkflow = (part, workflows, workflowName) ->
  if workflowName == "A"
    return true
  if workflowName == "R"
    return false
  for rule in workflows[workflowName]
    rating = part[rule.category]
    isValid = true
    switch rule.operator
      when '>'
        isValid = rating > rule.num
      when '<'
        isValid = rating < rule.num
    if isValid
      return applyWorkflow(part, workflows, rule.workflowName)
  false

solve = (input) ->
  startWorflow = "in"
  [workflows, parts] = parseInput(input)
  res = 0
  for part in parts
    if applyWorkflow(part, workflows, startWorflow)
      res += part.x + part.m + part.a + part.s
  res

readFile = (fileName) ->
  fs.readFileSync(fileName, 'utf-8').trim().split '\n'

input = readFile("input.txt")
console.log solve(input)
