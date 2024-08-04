fs = require 'fs'

class Step
  constructor: (@id, @duration) ->

parseInput = (filename) ->
  deps = {}
  allSteps = {}
  data = fs.readFileSync(filename, 'utf8')
  lines = data.split('\n')
  for line in lines
    match = line.match /Step (\w) must be finished before step (\w) can begin./
    if match
      deps[match[2]] ?= []
      deps[match[2]].push match[1]
      allSteps[match[1]] ?= new Step(match[1], match[1].charCodeAt(0) - 64 + 60)
      allSteps[match[2]] ?= new Step(match[2], match[2].charCodeAt(0) - 64 + 60)
  [deps, allSteps]

simulateWork = (deps, allSteps, numWorkers, baseDuration) ->
  workers = new Array(numWorkers).fill(0)
  tasks = new Array(numWorkers).fill(0)
  time = 0
  while Object.keys(allSteps).length > 0
    available = []
    for step of allSteps
      if (deps[step] or []).length is 0 and tasks.indexOf(step) is -1
        available.push step
    available.sort()
    for i in [0...numWorkers]
      if workers[i] is 0 and available.length > 0
        tasks[i] = available.shift()
        workers[i] = allSteps[tasks[i]].duration
    minDuration = Math.min(...workers.filter (x) -> x > 0)
    for i in [0...numWorkers]
      if workers[i] > 0
        workers[i] -= minDuration
        if workers[i] is 0
          finishStep(deps, allSteps, tasks[i])
          tasks[i] = 0
    time += minDuration
  time

finishStep = (deps, allSteps, step) ->
  delete allSteps[step]
  for s of allSteps
    deps[s] = (deps[s] or []).filter (x) -> x isnt step

[deps, allSteps] = parseInput('input.txt')
console.log simulateWork(deps, allSteps, 5, 60)