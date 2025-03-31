
import strutils, tables, sequtils, sets

# --- Type Definitions ---

type
  RuleKind = enum
    rkChar, rkSeq, rkAlt
  Rule = object
    case kind*: RuleKind
    of rkChar: charVal*: char
    of rkSeq: seqVal*: seq[int]
    of rkAlt: altVal*: seq[seq[int]]

# --- Global Variables ---
# Using Tables for efficient lookups
var rules: Table[int, Rule]
var messages: seq[string]
# Memoization cache for the matching function
# Key: (ruleId, startIndex), Value: Set of possible end indices
var memo: Table[(int, int), HashSet[int]]

# --- Parsing Logic ---

proc parseRule(line: string): (int, Rule) =
  let parts = line.split(": ")
  let ruleId = parseInt(parts[0])
  let definition = parts[1]

  if definition.startsWith('"'):
    # Character rule: "a"
    result = (ruleId, Rule(kind: rkChar, charVal: definition[1]))
  elif definition.contains('|'):
    # Alternative rule: 1 3 | 3 1
    var alternatives: seq[seq[int]]
    for altPart in definition.split(" | "):
      var seqRule: seq[int]
      for subRuleIdStr in altPart.split(' '):
        if subRuleIdStr.len > 0: # Handle potential empty strings from split
          seqRule.add(parseInt(subRuleIdStr))
      alternatives.add(seqRule)
    result = (ruleId, Rule(kind: rkAlt, altVal: alternatives))
  else:
    # Sequence rule: 1 2
    var seqRule: seq[int]
    for subRuleIdStr in definition.split(' '):
       if subRuleIdStr.len > 0: # Handle potential empty strings from split
        seqRule.add(parseInt(subRuleIdStr))
    result = (ruleId, Rule(kind: rkSeq, seqVal: seqRule))

proc readInput(filename: string) =
  rules = initTable[int, Rule]()
  messages = @[]
  var readingRules = true
  for line in lines(filename):
    if line.len == 0:
      readingRules = false
      continue

    if readingRules:
      let (id, rule) = parseRule(line)
      rules[id] = rule
    else:
      messages.add(line)

# --- Matching Logic with Memoization ---

proc matchRule(ruleId: int, message: string, startIndex: int): HashSet[int] =
  ## Recursively checks if the message matches the rule starting at startIndex.
  ## Returns a set of possible indices in the message *after* a successful match.
  ## An empty set indicates no match possible from startIndex.
  ## Uses memoization to avoid redundant computations.

  # Check memoization cache first
  let cacheKey = (ruleId, startIndex)
  if memo.contains(cacheKey):
    return memo[cacheKey]

  # Ensure startIndex is valid before proceeding
  if startIndex >= message.len:
    return initHashSet[int]() # Cannot match past the end of the message

  let rule = rules[ruleId] # Assume ruleId exists (valid input)
  var possibleEndIndices = initHashSet[int]()

  case rule.kind
  of rkChar:
    # Check if the character matches
    if message[startIndex] == rule.charVal:
      possibleEndIndices.incl(startIndex + 1)
  of rkSeq:
    # Must match all sub-rules in sequence
    var currentIndices: HashSet[int] = toHashSet([startIndex]) # Start with the initial index

    for subRuleId in rule.seqVal:
      var nextIndices = initHashSet[int]()
      if currentIndices.len == 0: # If previous step failed, cannot continue
         break
      for currentIndex in currentIndices:
        let subRuleMatches = matchRule(subRuleId, message, currentIndex)
        # Add all successful end positions from the sub-rule match
        for endIndex in subRuleMatches:
          nextIndices.incl(endIndex)
      currentIndices = nextIndices # Update the set of possible indices for the next sub-rule

    # The final set of indices after matching the whole sequence
    possibleEndIndices = currentIndices

  of rkAlt:
    # Try each alternative, collect all possible outcomes
    for alternativeSeq in rule.altVal:
       # Similar logic to rkSeq, but applied to one alternative
      var currentIndices: HashSet[int] = toHashSet([startIndex])

      for subRuleId in alternativeSeq:
        var nextIndices = initHashSet[int]()
        if currentIndices.len == 0:
           break
        for currentIndex in currentIndices:
          let subRuleMatches = matchRule(subRuleId, message, currentIndex)
          for endIndex in subRuleMatches:
            nextIndices.incl(endIndex)
        currentIndices = nextIndices

      # Add the results from this alternative to the total set
      for endIndex in currentIndices:
        possibleEndIndices.incl(endIndex)

  # Store result in memoization cache before returning
  memo[cacheKey] = possibleEndIndices
  result = possibleEndIndices

# --- Main Execution ---

proc main() =
  readInput("input.txt")

  var validMessageCount = 0
  for message in messages:
    # Clear memoization cache for each new message
    memo = initTable[(int, int), HashSet[int]]()
    let endIndices = matchRule(0, message, 0)

    # Check if any of the successful matches consumed the *entire* message
    if endIndices.contains(message.len):
      validMessageCount += 1

  echo validMessageCount

# --- Entry Point ---
when isMainModule:
  main()
