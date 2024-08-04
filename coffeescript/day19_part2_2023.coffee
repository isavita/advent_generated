fs = require 'fs'

parseInput = (input) ->
  workflows = {}
  parts = []
  i = 0
  while input[i] != ""
    [workflowName, rules] = parseWorkflow input[i]
    workflows[workflowName] = rules
    i++
  for i in [i+1...input.length]
    part = parsePart input[i]
    parts.push part
  [workflows, parts]

parseWorkflow = (line) ->
  idx = line.indexOf "{"
  workflowName = line.slice 0, idx
  rules = []
  rulesStr = line.slice(idx+1, line.length-1).split ","
  for ruleStr in rulesStr
    rule = {}
    idx = ruleStr.indexOf ":"
    if idx == -1
      rule.WorkflowName = ruleStr
    else
      rule.Category = ruleStr[0]
      rule.Operator = ruleStr[1]
      rule.Num = parseInt ruleStr.slice(2, idx)
      rule.WorkflowName = ruleStr.slice idx+1
    rules.push rule
  [workflowName, rules]

parsePart = (line) ->
  [_, x, m, a, s] = line.match /{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}/
  x: parseInt(x), m: parseInt(m), a: parseInt(a), s: parseInt(s)

applyWorkflow = (part, workflows, workflowName) ->
  if workflowName == "A"
    return true
  if workflowName == "R"
    return false
  for rule in workflows[workflowName]
    rating = part[rule.Category]
    isValid = switch rule.Operator
      when ">" then rating > rule.Num
      when "<" then rating < rule.Num
      else true
    if isValid
      return applyWorkflow part, workflows, rule.WorkflowName
  false

applyWorkflowInterval = (partInterval, workflows, workflowName) ->
  if workflowName == "A"
    res = 1
    for interval in Object.values partInterval
      res *= interval.End - interval.Start + 1
    return res
  if workflowName == "R"
    return 0
  res = 0
  for rule in workflows[workflowName]
    ratingInterval = partInterval[rule.Category]
    if rule.Operator == ">"
      validRatingInterval = {Start: rule.Num + 1, End: ratingInterval.End}
      invalidRatingInterval = {Start: ratingInterval.Start, End: rule.Num}
    else if rule.Operator == "<"
      validRatingInterval = {Start: ratingInterval.Start, End: rule.Num - 1}
      invalidRatingInterval = {Start: rule.Num, End: ratingInterval.End}
    else
      validRatingInterval = ratingInterval
      invalidRatingInterval = null
    newPart = {}
    for key, value of partInterval
      if key == rule.Category
        newPart[rule.Category] = validRatingInterval
      else
        newPart[key] = value
    res += applyWorkflowInterval newPart, workflows, rule.WorkflowName
    if invalidRatingInterval
      partInterval[rule.Category] = invalidRatingInterval
  res

solve = (input) ->
  startWorflow = "in"
  minRating = 1
  maxRating = 4000
  [workflows] = parseInput input
  partInterval =
    x: Start: minRating, End: maxRating
    m: Start: minRating, End: maxRating
    a: Start: minRating, End: maxRating
    s: Start: minRating, End: maxRating
  applyWorkflowInterval partInterval, workflows, startWorflow

readFile = (fileName) ->
  fs.readFileSync(fileName).toString().trim().split "\n"

input = readFile "input.txt"
console.log solve input