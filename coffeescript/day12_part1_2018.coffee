fs = require 'fs'

sumPots = (state) ->
  sum = 0
  for k, v of state
    sum += parseInt(k) if v is '#'
  sum

processFile = (filename) ->
  data = fs.readFileSync filename, 'utf8'
  lines = data.trim().split '\n'
  initialState = ''
  rules = {}

  for line in lines
    if line.includes 'initial state'
      initialState = line.split(': ')[1]
    else if line.includes '=>'
      parts = line.split ' => '
      rules[parts[0]] = parts[1]

  state = {}
  for i in [0...initialState.length]
    state[i] = initialState[i] if initialState[i] is '#'

  for generation in [0...20]
    newState = {}
    potNumbers = Object.keys(state).map (num) -> parseInt num
    minPot = Math.min.apply(null, potNumbers)
    maxPot = Math.max.apply(null, potNumbers)

    for i in [minPot-2..maxPot+2]
      pattern = ''
      for j in [i-2..i+2]
        pattern += if state[j]? then state[j] else '.'
      newState[i] = '#' if rules[pattern] is '#'

    state = newState

  console.log sumPots(state)

processFile 'input.txt'