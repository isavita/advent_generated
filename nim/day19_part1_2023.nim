
import strutils, streams, tables, sequtils

type
  Part = object
    x, m, a, s: int
  Rule = object
    category: char # '\0' for default rule
    op: char       # '<' or '>'
    value: int
    destination: string
  Workflows = Table[string, seq[Rule]]

proc parseWorkflows(lines: seq[string]): Workflows =
  result = initTable[string, seq[Rule]]()
  for line in lines:
    if line.len == 0: continue
    let parts = line.split('{')
    let name = parts[0]
    let rulesStr = parts[1][0 .. ^2] # Remove trailing '}'
    var rulesSeq: seq[Rule]
    for ruleStr in rulesStr.split(','):
      if ':' in ruleStr:
        let ruleParts = ruleStr.split(':')
        let condition = ruleParts[0]
        let destination = ruleParts[1]
        let category = condition[0]
        let op = condition[1]
        let value = parseInt(condition[2 .. ^1])
        rulesSeq.add(Rule(category: category, op: op, value: value, destination: destination))
      else:
        # Default rule
        rulesSeq.add(Rule(category: '\0', op: '\0', value: 0, destination: ruleStr))
    result[name] = rulesSeq

proc parseParts(lines: seq[string]): seq[Part] =
  for line in lines:
    if line.len == 0: continue
    var part: Part
    let ratingsStr = line[1 .. ^2] # Remove '{' and '}'
    for ratingPair in ratingsStr.split(','):
      let kv = ratingPair.split('=')
      let value = parseInt(kv[1])
      case kv[0][0]:
      of 'x': part.x = value
      of 'm': part.m = value
      of 'a': part.a = value
      of 's': part.s = value
      else: discard # Should not happen with valid input
    result.add(part)

proc evaluate(part: Part, rule: Rule): bool =
  let partValue = case rule.category
                  of 'x': part.x
                  of 'm': part.m
                  of 'a': part.a
                  of 's': part.s
                  else: 0 # Should only happen for default rule check, effectively false
  
  case rule.op:
  of '<': return partValue < rule.value
  of '>': return partValue > rule.value
  else: return false # Should not happen for non-default rules

proc processPart(workflows: Workflows, part: Part): bool =
  var currentWorkflow = "in"
  while true:
    if currentWorkflow == "A": return true
    if currentWorkflow == "R": return false

    if not workflows.contains(currentWorkflow):
        # Or handle error appropriately
        return false 

    let rules = workflows[currentWorkflow]
    var nextWorkflow = "" 
    
    for rule in rules:
      if rule.category == '\0': # Default rule
        nextWorkflow = rule.destination
        break
      elif evaluate(part, rule):
        nextWorkflow = rule.destination
        break
    
    if nextWorkflow.len == 0:
      # Should not happen if workflows are well-formed with a default rule
      return false # Consider raising an error

    currentWorkflow = nextWorkflow

proc main() =
  let fileStream = newFileStream("input.txt", fmRead)
  if fileStream == nil:
    echo "Error opening input file"
    return

  let content = fileStream.readAll()
  fileStream.close()

  let sections = content.split("\n\n")
  if sections.len != 2:
      echo "Invalid input format: Expected two sections separated by a blank line."
      return

  let workflowLines = sections[0].splitLines()
  # Strip potential trailing newline from the second section before splitting
  let partLines = sections[1].strip().splitLines() 

  let workflows = parseWorkflows(workflowLines)
  let parts = parseParts(partLines)

  var totalSum: int64 = 0 # Use int64 for potentially large sums
  for part in parts:
    if processPart(workflows, part):
      totalSum += int64(part.x) + int64(part.m) + int64(part.a) + int64(part.s)

  echo totalSum

main()
