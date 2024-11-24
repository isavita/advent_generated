
import std/[strutils, os, tables, re, parseutils]

type
  Program = object
    weight: int
    holds: seq[string]

proc dfs(name: string, programs: Table[string, Program]): tuple[weight: int, balanced: bool] =
  let program = programs[name]
  var totalWeight = program.weight

  var weights = initTable[int, int]()
  for child in program.holds:
    let (childWeight, balanced) = dfs(child, programs)
    if not balanced:
      return (0, false)
    
    totalWeight += childWeight
    weights[childWeight] = weights.getOrDefault(childWeight, 0) + 1

  # Check for unbalance
  for w1, c1 in weights:
    for w2, c2 in weights:
      if w1 != w2 and c1 < c2:
        var unbalancedProgram = ""
        for child in program.holds:
          let (childWeight, _) = dfs(child, programs)
          if childWeight == w1:
            unbalancedProgram = child
            break
        
        echo programs[unbalancedProgram].weight + (w2 - w1)
        return (0, false)

  return (totalWeight, true)

proc main() =
  # Read input
  let data = readFile("input.txt").strip()
  let lines = data.splitLines()

  # Create data structure
  var programs = initTable[string, Program]()

  for line in lines:
    let matches = line.findAll(re"[a-z]+|\d+")
    let name = matches[0]
    var weight: int
    discard parseInt(matches[1], weight)

    var program = Program(weight: weight)
    if matches.len > 2:
      program.holds = matches[2..^1]
    
    programs[name] = program

  # Find unbalance
  discard dfs("dtacyn", programs)

main()
