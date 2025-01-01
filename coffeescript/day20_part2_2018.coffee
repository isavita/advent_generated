
fs = require 'fs'

buildMap = (regex) ->
  dm = {}
  stack = []
  cp = {x: 0, y: 0}
  for char in regex
    switch char
      when '('
        stack.push {x: cp.x, y: cp.y}
      when '|'
        cp = stack[stack.length - 1]
      when ')'
        cp = stack.pop()
      else
        np = move cp, char
        dm[cp.x + ',' + cp.y] ?= {}
        dm[cp.x + ',' + cp.y][np.x + ',' + np.y] = true
        cp = np
  dm

move = (p, dir) ->
  switch dir
    when 'N' then {x: p.x, y: p.y - 1}
    when 'S' then {x: p.x, y: p.y + 1}
    when 'E' then {x: p.x + 1, y: p.y}
    when 'W' then {x: p.x - 1, y: p.y}

countRooms = (dm, minDoors) ->
  visited = {}
  queue = [{x: 0, y: 0, dist: 0}]
  roomCount = 0
  while queue.length > 0
    current = queue.shift()
    p = {x: current.x, y: current.y}
    key = p.x + ',' + p.y
    if visited[key]
      continue
    visited[key] = current.dist
    if current.dist >= minDoors
      roomCount++
    if dm[key]
      for nextKey of dm[key]
        [nx, ny] = nextKey.split(',').map(Number)
        queue.push {x: nx, y: ny, dist: current.dist + 1}
  roomCount

data = fs.readFileSync('input.txt', 'utf8').trim()
regex = data.slice(1, -1)
dm = buildMap regex
rooms = countRooms dm, 1000
console.log rooms
