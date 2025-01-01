
fs = require 'fs'

readAll = (path) ->
  fs.readFileSync(path, 'utf-8').trim()

parseInput = (input) ->
  valves = {}
  for line in input.split('\n')
    parts = line.split('; ')
    match = parts[0].match(/Valve (\w+) has flow rate=(\d+)/)
    id = match[1]
    flow = parseInt(match[2])
    tunnels = {}
    tunnels[id] = 0
    tunnelPart = parts[1].replace('tunnel leads to valve', '').replace('tunnels lead to valves', '').replace('s', '').trim()
    for tunnel in tunnelPart.split(', ')
      tunnels[tunnel] = 1
    valves[id] = {id, flow, tunnels}
  valves

floydWarshall = (valves) ->
  for k, _ of valves
    for i, _ of valves
      for j, _ of valves
        if valves[i].tunnels[k]? and valves[k].tunnels[j]?
          dist = (valves[i].tunnels[k] or 0) + (valves[k].tunnels[j] or 0)
          if not valves[i].tunnels[j]? or valves[i].tunnels[j] > dist
            valves[i].tunnels[j] = dist
  valves

getOpenValves = (valves) ->
  (v.id for k, v of valves when v.flow > 0)

divide = (l) ->
  if l == 1
    return [
      [[], [0]]
      [[0], []]
    ]
  d = divide(l - 1)
  r = []
  for pair in d
    r.push [[l - 1, ...pair[0]], pair[1]]
    r.push [pair[0], [l - 1, ...pair[1]]]
  r

maxPressure = (valves, curr, minute, pressure, open, d, cache = {}) ->
  key = "#{curr},#{minute},#{pressure},#{open.join(',')}"
  return cache[key] if cache[key]?
  max = pressure
  for next, i in open
    newOpen = open.filter((_, index) -> index != i)
    timeLeft = minute - valves[curr].tunnels[next] - 1
    if timeLeft > 0
      max = Math.max(max, maxPressure(valves, next, timeLeft, timeLeft * valves[next].flow + pressure, newOpen, d + 1, cache))
  cache[key] = max
  max

solve = (input) ->
  valves = parseInput(input)
  valves = floydWarshall(valves)
  openValves = getOpenValves(valves)
  max = 0
  for d in divide(openValves.length)
    if d[0].length == 0 or d[1].length == 0
      continue
    mine = (openValves[i] for i in d[0])
    elephant = (openValves[i] for i in d[1])
    x = maxPressure(valves, 'AA', 26, 0, mine, 0) + maxPressure(valves, 'AA', 26, 0, elephant, 0)
    max = Math.max(max, x)
  console.log max

input = readAll('input.txt')
solve(input)
