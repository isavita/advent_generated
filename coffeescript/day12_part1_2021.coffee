fs = require 'fs'

class Cave
  constructor: ->
    @connections = {}

  connectTo: (name) ->
    @connections[name] = true

  disconnectFrom: (name) ->
    delete @connections[name]

dfs = (current, visited, caves, count) ->
  return count + 1 if current == 'end'

  newCount = count
  for next of caves[current].connections
    continue if visited[next] and next.toLowerCase() == next
    visitedCopy = {}
    visitedCopy[k] = v for k, v of visited
    visitedCopy[next] = true
    newCount = dfs(next, visitedCopy, caves, newCount)

  newCount

caves = {}
data = fs.readFileSync('input.txt', 'utf8')
lines = data.trim().split('\n')
for line in lines
  [from, to] = line.split('-')
  caves[from] ?= new Cave()
  caves[to] ?= new Cave()
  caves[from].connectTo(to)
  caves[to].connectTo(from)

result = dfs('start', {'start': true}, caves, 0)
console.log result