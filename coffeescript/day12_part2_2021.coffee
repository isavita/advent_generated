
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8').trim().split('\n')

connections = {}

for line in input
  [a, b] = line.split('-')
  connections[a] ?= []
  connections[b] ?= []
  connections[a].push(b)
  connections[b].push(a)

isSmallCave = (cave) -> cave.toLowerCase() == cave

findPaths = (allowDoubleVisit) ->
  paths = 0
  
  traverse = (currentCave, visited, doubleVisited) ->
    if currentCave == 'end'
      paths++
      return
    
    for nextCave in connections[currentCave]
      if nextCave == 'start'
        continue
      
      if isSmallCave(nextCave)
        if visited[nextCave]
          if allowDoubleVisit and not doubleVisited
            newVisited = Object.assign({}, visited)
            newVisited[nextCave]++
            traverse(nextCave, newVisited, true)
          else
            continue
        else
          newVisited = Object.assign({}, visited)
          newVisited[nextCave] = 1
          traverse(nextCave, newVisited, doubleVisited)
      else
        traverse(nextCave, visited, doubleVisited)
  
  traverse('start', {}, false)
  paths

part1 = findPaths(false)
console.log "Part 1:", part1

part2 = findPaths(true)
console.log "Part 2:", part2
