
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf-8').trim()
regex = input.slice(1, -1)

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
        np = move(cp, char)
        dm[JSON.stringify(cp)] ?= {}
        dm[JSON.stringify(cp)][JSON.stringify(np)] = true
        cp = np
  dm

move = (p, dir) ->
  switch dir
    when 'N' then {x: p.x, y: p.y - 1}
    when 'S' then {x: p.x, y: p.y + 1}
    when 'E' then {x: p.x + 1, y: p.y}
    when 'W' then {x: p.x - 1, y: p.y}

findFurthestRoom = (dm) ->
  visited = {}
  queue = [{x: 0, y: 0, dist: 0}]
  maxDoors = 0
  while queue.length > 0
    current = queue.shift()
    p = {x: current.x, y: current.y}
    if visited[JSON.stringify(p)]
      continue
    visited[JSON.stringify(p)] = current.dist
    maxDoors = Math.max(maxDoors, current.dist)
    if dm[JSON.stringify(p)]
      for np_str of dm[JSON.stringify(p)]
        np = JSON.parse(np_str)
        queue.push {x: np.x, y: np.y, dist: current.dist + 1}
  maxDoors

dm = buildMap(regex)
maxDoors = findFurthestRoom(dm)
console.log maxDoors
