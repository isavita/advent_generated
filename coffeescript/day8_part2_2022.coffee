fs = require 'fs'

neighbors4 = [{x: 0, y: 1}, {x: 0, y: -1}, {x: 1, y: 0}, {x: -1, y: 0}]

processInput = (data) ->
  grid = {}
  lines = data.split '\n'
  for y in [0...lines.length]
    for x in [0...lines[y].length]
      grid["#{x},#{y}"] = parseInt lines[y][x]

  maxScore = 0
  for key, value of grid
    [x, y] = key.split(',').map (num) -> parseInt num
    score = 1
    for neighbor in neighbors4
      dx = neighbor.x
      dy = neighbor.y
      nextX = x + dx
      nextY = y + dy
      nextKey = "#{nextX},#{nextY}"
      view = 0
      while nextKey of grid
        view++
        if grid[nextKey] >= grid[key]
          score *= view
          break
        nextX += dx
        nextY += dy
        nextKey = "#{nextX},#{nextY}"
      score *= view unless nextKey of grid

    maxScore = Math.max maxScore, score

  console.log maxScore

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.error err
  else
    processInput data