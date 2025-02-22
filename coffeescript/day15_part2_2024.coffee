fs = require 'fs'

posKey = (p) -> "#{p.x},#{p.y}"
add = (p, d) -> {x: p.x + d.x, y: p.y + d.y}
clone = (m) ->
  orig = {}
  for k, v of m
    orig[k] = v
  orig

tryToStep = (m, pos, dir) ->
  orig = clone m
  k = posKey pos
  c = m[k]
  if c == '.'
    return true
  else if c == 'O' or c == '@'
    if tryToStep(m, add(pos, dir), dir)
      m[posKey(add(pos, dir))] = c
      m[k] = '.'
      return true
  else if c == ']'
    if tryToStep(m, add(pos, {x: -1, y: 0}), dir)
      return true
  else if c == '['
    if dir.x == -1 and dir.y == 0
      if tryToStep(m, add(pos, {x: -1, y: 0}), dir)
        m[posKey(add(pos, {x: -1, y: 0}))] = '['
        m[k] = ']'
        m[posKey(add(pos, {x: 1, y:0}))] = '.'
        return true
    else if dir.x == 1 and dir.y == 0
      if tryToStep(m, add(pos, {x: 2, y: 0}), dir)
        m[k] = '.'
        m[posKey(add(pos, {x:1, y:0}))] = '['
        m[posKey(add(pos, {x:2, y:0}))] = ']'
        return true
    else
      if tryToStep(m, add(pos, dir), dir) and tryToStep(m, add(add(pos, {x:1, y:0}), dir), dir)
        m[k] = '.'
        m[posKey(add(pos, {x:1, y:0}))] = '.'
        m[posKey(add(pos, dir))] = '['
        m[posKey(add(add(pos, {x:1, y:0}), dir))] = ']'
        return true
  for key of Object.keys m
    delete m[key]
  for key, value of orig
    m[key] = value
  false

solve = (inputStr) ->
  blocks = inputStr.split "\n\n"
  lines = blocks[0].split "\n"
  m = {}
  for y, row of lines
    for x, ch of row.split ''
      m["#{x},#{y}"] = ch
  steps = []
  for ch in blocks[1].replace(/\n/g, '').split ''
    if ch is '^'
      steps.push {x:0, y:-1}
    else if ch is '<'
      steps.push {x:-1, y:0}
    else if ch is '>'
      steps.push {x:1, y:0}
    else if ch is 'v'
      steps.push {x:0, y:1}
  robot = null
  for key, v of m
    if v is '@'
      parts = key.split ','
      robot = {x: parseInt(parts[0],10), y: parseInt(parts[1],10)}
      break
  for step in steps
    if tryToStep(m, robot, step)
      robot = add(robot, step)
  total = 0
  for key, v of m
    if v is '[' or v is 'O'
      parts = key.split ','
      x = parseInt(parts[0],10)
      y = parseInt(parts[1],10)
      total += x + 100 * y
  total

scaleUp = (inputStr) ->
  inputStr.replace(/#/g, "##").replace(/\./g, "..").replace(/O/g, "[]").replace(/@/g, "@.")

inputStr = fs.readFileSync "input.txt", "utf8"
console.log parseInt(solve(inputStr))
console.log parseInt(solve(scaleUp(inputStr)))