
fs = require('fs')
readline = require('readline')

rl = readline.createInterface({
  input: fs.createReadStream('input.txt'),
  terminal: false
})

initialState = ''
rules = {}

rl.on('line', (line) ->
  if line.includes('initial state')
    initialState = line.split(': ')[1]
  else if line.includes('=>')
    parts = line.split(' => ')
    rules[parts[0]] = parts[1][0]
)

rl.on('close', () ->
  state = {}
  for i, c of initialState
    if c == '#'
      state[i] = '#'

  previousPattern = ''
  previousSum = 0
  offset = 0
  for generation in [0...50000000000]
    newState = {}
    [minPot, maxPot] = minMaxKeys(state)
    for i in [minPot - 2...maxPot + 3]
      pattern = ''
      for j in [i - 2...i + 3]
        if state[j] == '#'
          pattern += '#'
        else
          pattern += '.'
      if rules[pattern] == '#'
        newState[i] = '#'
    state = newState

    [currentPattern, currentSum] = statePattern(state)
    if currentPattern == previousPattern
      offset = currentSum - previousSum
      remainingGenerations = 50000000000 - generation - 1
      finalSum = currentSum + offset * remainingGenerations
      console.log finalSum
      return
    previousPattern = currentPattern
    previousSum = currentSum
)

minMaxKeys = (m) ->
  minKey = Infinity
  maxKey = -Infinity
  for k, _ of m
    minKey = Math.min(minKey, k)
    maxKey = Math.max(maxKey, k)
  [minKey, maxKey]

statePattern = (m) ->
  [minPot, maxPot] = minMaxKeys(m)
  pattern = ''
  sum = 0
  for i in [minPot...maxPot + 1]
    if m[i] == '#'
      pattern += '#'
      sum += i
    else
      pattern += '.'
  [pattern, sum]
