
import strutils
import tables
import os

proc memoDfs(graph: Table[string, string], entry: string, memo: var Table[string, uint16]): uint16 =
  if entry in memo:
    return memo[entry]

  # Check if entry is a literal number
  try:
    let val = entry.parseUint.uint16
    memo[entry] = val
    return val
  except ValueError:
    discard # Not a simple number, proceed to parse rule

  let sourceRule = graph[entry]
  let parts = sourceRule.split(' ')

  var result: uint16 = 0
  if parts.len == 1:
    result = memoDfs(graph, parts[0], memo)
  elif parts[0] == "NOT":
    let operand = memoDfs(graph, parts[1], memo)
    result = not operand # Bitwise NOT for uint16
  elif parts[1] == "AND":
    let left = memoDfs(graph, parts[0], memo)
    let right = memoDfs(graph, parts[2], memo)
    result = left and right
  elif parts[1] == "OR":
    let left = memoDfs(graph, parts[0], memo)
    let right = memoDfs(graph, parts[2], memo)
    result = left or right
  elif parts[1] == "LSHIFT":
    let left = memoDfs(graph, parts[0], memo)
    let right = memoDfs(graph, parts[2], memo) # Shift amount can also be a wire
    result = left shl right
  elif parts[1] == "RSHIFT":
    let left = memoDfs(graph, parts[0], memo)
    let right = memoDfs(graph, parts[2], memo) # Shift amount can also be a wire
    result = left shr right
  # No else needed assuming valid input format

  memo[entry] = result
  return result

when isMainModule:
  let input = readFile("input.txt").strip()
  var wireToRule = initTable[string, string]()

  for line in input.splitLines():
    if line.len > 0:
      let ruleParts = line.split(" -> ")
      wireToRule[ruleParts[1]] = ruleParts[0]

  # Part 1: Calculate initial signal 'a'
  var memo1 = initTable[string, uint16]()
  let aSignal = memoDfs(wireToRule, "a", memo1)

  # Part 2: Override wire 'b' with signal 'a' and recalculate 'a'
  wireToRule["b"] = $aSignal # Use string interpolation ($) to convert uint16 to string
  var memo2 = initTable[string, uint16]() # Reset memoization cache
  let finalASignal = memoDfs(wireToRule, "a", memo2)

  echo finalASignal

