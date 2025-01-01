
fs = require 'fs'

parseInput = (filePath) ->
  lines = fs.readFileSync(filePath, 'utf-8').trim().split('\n')
  initialState = lines[0].slice(-2, -1)
  steps = parseInt(lines[1].match(/\d+/)[0])
  states = {}
  i = 3
  while i < lines.length
    state = lines[i].slice(-2, -1)
    value0 = parseInt(lines[i + 2].slice(-2, -1))
    move0 = if lines[i + 3].endsWith('left.') then -1 else 1
    nextState0 = lines[i + 4].slice(-2, -1)
    value1 = parseInt(lines[i + 6].slice(-2, -1))
    move1 = if lines[i + 7].endsWith('left.') then -1 else 1
    nextState1 = lines[i + 8].slice(-2, -1)
    states[state] = {0: [value0, move0, nextState0], 1: [value1, move1, nextState1]}
    i += 10
  [initialState, steps, states]

runTuringMachine = (filePath) ->
  [state, steps, states] = parseInput(filePath)
  tape = {}
  cursor = 0
  checksum = 0
  for _ in [0...steps]
    value = tape[cursor] ? 0
    [newValue, move, nextState] = states[state][value]
    tape[cursor] = newValue
    cursor += move
    state = nextState
  for v of tape
    checksum += tape[v]
  checksum

result = runTuringMachine('input.txt')
console.log result
