fs = require 'fs'

class Point
  constructor: (@x, @y) ->

class State
  constructor: (@pos, @keys) ->

findShortestPath = (grid, start, keyMap) ->
  dirs = [new Point(0, -1), new Point(-1, 0), new Point(0, 1), new Point(1, 0)]
  visited = {}
  queue = [new State(start, 0)]
  steps = 0

  while queue.length > 0
    size = queue.length
    for i in [0...size]
      current = queue.shift()

      return steps if current.keys == (1 << Object.keys(keyMap).length) - 1

      for d in dirs
        next = new Point(current.pos.x + d.x, current.pos.y + d.y)
        if next.x >= 0 and next.x < grid[0].length and next.y >= 0 and next.y < grid.length
          char = grid[next.y][next.x]
          if char isnt '#' and not (char >= 'A' and char <= 'Z' and (current.keys & (1 << keyMap[String.fromCharCode(char.charCodeAt(0) + 32)])) == 0)
            newState = new State(next, current.keys)
            newState.keys |= (1 << keyMap[char]) if char >= 'a' and char <= 'z'
            stateKey = "#{newState.pos.x},#{newState.pos.y},#{newState.keys}"
            unless visited[stateKey]
              visited[stateKey] = true
              queue.push newState
    steps++

  return -1

fs.readFile 'input.txt', 'utf8', (err, data) ->
  grid = []
  start = null
  keyMap = {}
  keyCounter = 0

  y = 0
  for line in data.split '\n'
    grid.push line
    for x in [0...line.length]
      if line[x] == '@'
        start = new Point(x, y)
      else if line[x] >= 'a' and line[x] <= 'z'
        unless keyMap[line[x]]
          keyMap[line[x]] = keyCounter
          keyCounter++

    y++

  console.log findShortestPath(grid, start, keyMap)