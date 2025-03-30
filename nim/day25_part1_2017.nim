
import strutils
import tables
import os
import sequtils

type
  StateRule = tuple[writeVal: int, moveDir: int, nextState: char]
  StateTransitions = Table[int, StateRule]
  Blueprint = tuple[initialState: char, steps: int, states: Table[char, StateTransitions]]

proc readBlueprint(filename: string): Blueprint =
  let content = readFile(filename)
  var lines = content.strip.splitLines()

  # Filter out empty lines
  lines = lines.filter(proc(s: string): bool = s.len > 0)

  let initialStateLine = lines[0].split()
  result.initialState = initialStateLine[^1][0] # First char of last word

  let stepsLine = lines[1].split()
  result.steps = parseInt(stepsLine[^2]) # Second to last word

  result.states = initTable[char, StateTransitions]()

  var i = 2
  while i < lines.len:
    let stateName = lines[i][^2] # Second last char
    i += 1
    var transitions = initTable[int, StateRule]()

    for _ in 0..<2:
      let valConditionLine = lines[i].split()
      let currentVal = parseInt($valConditionLine[^1][0]) # First char of last word
      i += 1

      let writeValLine = lines[i].split()
      let writeVal = parseInt($writeValLine[^1][0]) # First char of last word
      i += 1

      let moveDirLine = lines[i].split()
      let moveDir = if "left" in moveDirLine[^1]: -1 else: 1
      i += 1

      let nextStateLine = lines[i].split()
      let nextState = nextStateLine[^1][0] # First char of last word, remove trailing '.' implicitly
      i += 1

      transitions[currentVal] = (writeVal: writeVal, moveDir: moveDir, nextState: nextState)

    result.states[stateName] = transitions

proc runTuringMachine(blueprint: Blueprint): int =
  var tape = initTable[int, int]()
  var cursor = 0
  var state = blueprint.initialState

  for _ in 0..<blueprint.steps:
    let currentVal = tape.getOrDefault(cursor, 0)
    let rule = blueprint.states[state][currentVal]

    tape[cursor] = rule.writeVal
    cursor += rule.moveDir
    state = rule.nextState

  # Calculate checksum
  result = 0
  for val in tape.values:
      result += val
  # Alternative using sequtils.sum (might need `import std/sums` in older Nim versions)
  # result = tape.values.toSeq.sum

proc main() =
  if fileExists("input.txt"):
    let blueprint = readBlueprint("input.txt")
    let checksum = runTuringMachine(blueprint)
    echo checksum
  else:
    echo "Error: input.txt not found."

when isMainModule:
  main()
