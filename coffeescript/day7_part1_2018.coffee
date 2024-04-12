fs = require 'fs'

parseInput = (filename) ->
  deps = {}
  allSteps = {}
  data = fs.readFileSync filename, 'utf8'
  lines = data.trim().split '\n'
  for line in lines
    [_, a, b] = /^Step (\w) must be finished before step (\w) can begin.$/.exec(line)
    deps[b] = (deps[b] or []).concat a
    allSteps[a] = true
    allSteps[b] = true
  return [deps, allSteps]

topologicalSort = (deps, allSteps) ->
  order = []
  available = []
  for step of allSteps
    available.push step if not deps[step]?.length
  available.sort()

  while available.length > 0
    next = available.shift()
    order.push next
    for step of allSteps
      if deps[step]?.includes next
        deps[step].splice deps[step].indexOf(next), 1
        available.push step unless deps[step].length or available.includes step
    available.sort()

  order.join ''

[deps, allSteps] = parseInput 'input.txt'
order = topologicalSort deps, allSteps
console.log order