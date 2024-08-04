fs = require 'fs'

class FlipFlop
  constructor: (@name, @moduleType, @connectsTo, @state = false) ->

class Conjunction
  constructor: (@name, @moduleType, @connectsTo, @watches = {}) ->

class Broadcaster
  constructor: (@name, @moduleType, @connectsTo) ->

BROADCASTER = 0
FLIP_FLOP = 1
CONJUNCTION = 2

handleLine = (line, connections) ->
  if line.includes 'broadcaster'
    [name, rest] = line.split ' -> '
    module = new Broadcaster name, BROADCASTER, rest.split ', '
  else if line.includes '%'
    [name, rest] = line.split ' -> '
    module = new FlipFlop name.slice(1), FLIP_FLOP, rest.split ', '
  else
    [name, rest] = line.split ' -> '
    module = new Conjunction name.slice(1), CONJUNCTION, rest.split ', '
  connections[module.name] = module

completeWatches = (connections) ->
  for name, module of connections
    if module instanceof Conjunction
      for name2, module2 of connections
        if module2 instanceof FlipFlop or module2 instanceof Conjunction
          for conn in module2.connectsTo
            if conn == module.name
              module.watches[module2.name] = false
      connections[module.name] = module

class State
  constructor: (@from, @name, @pulse) ->

simulatePress = (connections, loops, pressNumber) ->
  queue = [new State 'button', 'broadcaster', false]
  pulses = [1, 0]
  found = false

  while queue.length > 0
    currState = queue.shift()
    module = connections[currState.name]

    if currState.name == 'out'
      continue

    if currState.name == 'rx' and not currState.pulse
      found = true

    pulse = currState.pulse

    if module instanceof Broadcaster
      for name in module.connectsTo
        queue.push new State module.name, name, pulse
        pulses[if pulse then 1 else 0]++
    else if module instanceof FlipFlop
      if not pulse
        module.state = not module.state
        for name in module.connectsTo
          queue.push new State module.name, name, module.state
          pulses[if module.state then 1 else 0]++
    else if module instanceof Conjunction
      module.watches[currState.from] = pulse
      allTrue = true
      for state of module.watches
        if not module.watches[state]
          allTrue = false
          break
      for name in module.connectsTo
        queue.push new State module.name, name, not allTrue
        pulses[if allTrue then 0 else 1]++

      if loops[currState.name] == -1 and not allTrue
        loops[currState.name] = pressNumber

  [pulses, found]

connectsTo = (from, to, connections) ->
  module = connections[from]
  module.connectsTo.includes to

copyConnections = (connections) ->
  copy = {}
  for k, v of connections
    copy[k] = v
  copy

includedInHistory = (hist, connections) ->
  for histConnections, i in hist
    if Object.keys(histConnections).length == Object.keys(connections).length and JSON.stringify(histConnections) == JSON.stringify(connections)
      return i
  -1

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.error 'Error reading file:', err
    return

  connections = {}
  lines = data.split '\n'
  for line in lines
    handleLine line, connections

  completeWatches connections

  pxPrev = []
  for k of connections
    if connectsTo k, 'rx', connections
      pxPrev.push k

  if pxPrev.length != 1
    throw new Error 'Error: more than one pxPrev'

  conj = connections[pxPrev[0]]

  loopLengths = {}
  for name of conj.watches
    loopLengths[name] = -1

  pressNumber = 0
  loop
    pressNumber++
    result = simulatePress connections, loopLengths, pressNumber
    pulses = result[0]
    found = result[1]
    if found
      break
    complete = true
    for length of loopLengths
      if loopLengths[length] == -1
        complete = false
        break
    if complete
      break

  sum = 1
  for length of loopLengths
    sum *= loopLengths[length]

  console.log sum