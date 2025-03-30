
import strutils, sequtils, tables, sets, math

const InputFile = "input.txt"

type
  Rule = object
    name: string
    ranges: array[2, tuple[low, high: int]]

proc isValid(rule: Rule, value: int): bool =
  for r in rule.ranges:
    if value >= r.low and value <= r.high:
      return true
  return false

proc parseTicket(s: string): seq[int] =
  result = newSeq[int]()
  for valStr in s.split(','):
    result.add parseInt(valStr)

proc isValidTicket(ticket: seq[int], rules: seq[Rule]): bool =
  for value in ticket:
    var foundRule = false
    for rule in rules:
      if rule.isValid(value):
        foundRule = true
        break
    if not foundRule:
      return false
  return true

proc solveFieldPositions(rules: seq[Rule], tickets: seq[seq[int]]): Table[string, int] =
  var validPositions = initTable[string, HashSet[int]]()
  let numFields = tickets[0].len
  for rule in rules:
    var possibleIndices = initHashSet[int]()
    for i in 0..<numFields:
      possibleIndices.incl(i)
    validPositions[rule.name] = possibleIndices

  for ticket in tickets:
    for idx, value in ticket:
      for rule in rules:
        if not rule.isValid(value):
          validPositions[rule.name].excl(idx)

  result = initTable[string, int]()
  var assignedPositions = initHashSet[int]()

  while result.len < rules.len:
    var foundRule: string = ""
    var foundPos: int = -1

    for name, positions in validPositions.pairs:
      if positions.len == 1:
        let pos = positions.toSeq[0] # Get the single element
        if pos notin assignedPositions:
            foundRule = name
            foundPos = pos
            break # Found a uniquely determined field

    if foundRule != "":
        result[foundRule] = foundPos
        assignedPositions.incl(foundPos)
        validPositions.del(foundRule) # Remove the determined rule

        # Remove the assigned position from other rules' possibilities
        for name in validPositions.keys:
            validPositions[name].excl(foundPos)
    else:
        # Should not happen with valid input according to AoC puzzle constraints
        # If it does, there's an issue with logic or input ambiguity
        break


proc calculateDepartureProduct(ticket: seq[int], fieldPositions: Table[string, int]): int64 =
  result = 1'i64
  for name, pos in fieldPositions.pairs:
    if name.startsWith("departure"):
      result *= ticket[pos].int64

proc main() =
  let content = readFile(InputFile)
  let lines = content.splitLines()

  var rules = newSeq[Rule]()
  var myTicket: seq[int]
  var nearbyTickets = newSeq[seq[int]]()

  var section = 0
  for line in lines:
    if line.len == 0:
      section += 1
      continue

    case section
    of 0: # Rules
      let parts = line.split(": ")
      let name = parts[0]
      let rangeParts = parts[1].split(" or ")
      var ruleRanges: array[2, tuple[low, high: int]]
      for i, rangeStr in rangeParts:
        let limits = rangeStr.split('-')
        ruleRanges[i] = (parseInt(limits[0]), parseInt(limits[1]))
      rules.add Rule(name: name, ranges: ruleRanges)
    of 1: # Your ticket
      if line != "your ticket:":
        myTicket = parseTicket(line)
    of 2: # Nearby tickets
      if line != "nearby tickets:":
        let ticket = parseTicket(line)
        if isValidTicket(ticket, rules):
          nearbyTickets.add(ticket)
    else: discard

  if nearbyTickets.len > 0:
      let fieldPositions = solveFieldPositions(rules, nearbyTickets)
      let departureProduct = calculateDepartureProduct(myTicket, fieldPositions)
      echo departureProduct
  else:
      echo "No valid nearby tickets found." # Handle edge case or indicate error


when isMainModule:
  main()
