fs = require 'fs'
solve = ->
  input = fs.readFileSync("input.txt", "utf8")
  lines = input.split("\n")
  graph = (line.split("") for line in lines when line.trim() != "")
  H = graph.length
  W = if H > 0 then graph[0].length else 0
  moves = [[-1,0],[0,-1],[1,0],[0,1]]
  total_sum = 0
  for y in [0...H]
    for x in [0...W]
      continue if graph[y][x] is '.'
      area = 0
      target = graph[y][x]
      visited = {}
      side = 
        left: []
        up: []
        right: []
        down: []
      q = [[x,y,""]]
      while q.length > 0
        [cx,cy,label] = q.shift()
        if cx < 0 or cx >= W or cy < 0 or cy >= H
          add_outer(label, side, cx, cy) if label?
          continue
        if graph[cy][cx] isnt target
          if label? and not visited["#{cx},#{cy}"]
            add_outer(label, side, cx, cy)
          continue
        visited["#{cx},#{cy}"] = true
        area++
        graph[cy][cx] = '.'
        for [dx,dy] in moves
          q.push [cx+dx, cy+dy, get_label(dx,dy)]
      outer = count_outer(side)
      total_sum += area * outer
  console.log total_sum

add_outer = (label, side, x, y) ->
  key = if label in ["up","down"] then {i: y, j: x} else {i: x, j: y}
  exists = false
  for k in side[label]
    if k.i is key.i and k.j is key.j
      exists = true
      break
  side[label].push key unless exists

count_outer = (side) ->
  outer = 0
  for label, keys of side
    sorted_keys = keys.sort (a,b) ->
      if a.i isnt b.i then a.i - b.i else a.j - b.j
    temp = []
    for key in sorted_keys
      outer++ unless check(temp, key)
      temp.push key
  outer

check = (ary, key) ->
  for [di,dj] in [[0,-1],[0,1]]
    neighbor = {i: key.i + di, j: key.j + dj}
    for item in ary
      return true if item.i is neighbor.i and item.j is neighbor.j
  false

get_label = (dx, dy) ->
  if dx is -1 then "left"
  else if dx is 1 then "right"
  else if dy is -1 then "up"
  else "down"

solve()