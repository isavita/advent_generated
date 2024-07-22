
import os, strutils, tables

proc readInput(filename: string): (string, Table[string, char]) =
  var initialState = ""
  var rules = initTable[string, char]()
  for line in lines(filename):
    if "initial state" in line:
      initialState = line.split(": ")[1]
    elif "=>" in line:
      let parts = line.split(" => ")
      rules[parts[0]] = parts[1][0]
  return (initialState, rules)

proc minMaxKeys(state: Table[int, char]): (int, int) =
  var minKey = 0
  var maxKey = 0
  for k in state.keys:
    if minKey == 0 and maxKey == 0:
      minKey = k
      maxKey = k
    else:
      if k < minKey: minKey = k
      if k > maxKey: maxKey = k
  return (minKey, maxKey)

proc statePattern(state: Table[int, char]): (string, int) =
  var pattern = ""
  var sum = 0
  let (minPot, maxPot) = minMaxKeys(state)
  for i in minPot..maxPot:
    if state.contains(i) and state[i] == '#':
      pattern.add('#')
      sum += i
    else:
      pattern.add('.')
  return (pattern, sum)

proc main() =
  let (initialState, rules) = readInput("input.txt")
  var state = initTable[int, char]()
  for i in 0..<initialState.len:
    if initialState[i] == '#':
      state[i] = '#'

  var previousPattern = ""
  var previousSum = 0
  var offset = 0
  for generation in 0..49999999999:
    var newState = initTable[int, char]()
    let (minPot, maxPot) = minMaxKeys(state)
    for i in (minPot - 2)..(maxPot + 2):
      var pattern = ""
      for j in (i - 2)..(i + 2):
        pattern.add(if state.contains(j) and state[j] == '#' : '#' else: '.')
      if rules.contains(pattern) and rules[pattern] == '#':
        newState[i] = '#'
    state = newState

    let (currentPattern, currentSum) = statePattern(state)
    if currentPattern == previousPattern:
      offset = currentSum - previousSum
      let remainingGenerations = 50000000000 - generation - 1
      let finalSum = currentSum + offset * remainingGenerations
      echo finalSum
      return
    previousPattern = currentPattern
    previousSum = currentSum

main()
