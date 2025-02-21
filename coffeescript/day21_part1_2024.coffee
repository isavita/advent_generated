
fs = require 'fs'

findPosition = (mat, ch) ->
  for i in [0...mat.length]
    for j in [0...mat[i].length]
      return [i, j] if mat[i][j] is ch
  [-1, -1]

ok = (mat, st, seq) ->
  [curr_i, curr_j] = st
  for ch in seq
    return false if mat[curr_i][curr_j] is ' '
    if ch is '^' then curr_i -= 1
    else if ch is 'v' then curr_i += 1
    else if ch is '<' then curr_j -= 1
    else if ch is '>' then curr_j += 1
    return false if curr_i < 0 or curr_i >= mat.length or curr_j < 0 or curr_j >= mat[0].length
  true

generateMoves = (position, objective, pad) ->
  [obj_i, obj_j] = findPosition pad, objective
  [pos_i, pos_j] = position
  ret = ""
  ret += '<'.repeat(pos_j - obj_j) if pos_j > obj_j
  ret += '^'.repeat(pos_i - obj_i) if pos_i > obj_i
  ret += 'v'.repeat(obj_i - pos_i) if pos_i < obj_i
  ret += '>'.repeat(obj_j - pos_j) if pos_j < obj_j
  if not ok pad, position, ret
    ret = ""
    ret += '>'.repeat(obj_j - pos_j) if pos_j < obj_j
    ret += '^'.repeat(pos_i - obj_i) if pos_i > obj_i
    ret += 'v'.repeat(obj_i - pos_i) if pos_i < obj_i
    ret += '<'.repeat(pos_j - obj_j) if pos_j > obj_j
  ret

solve = (code, robots, keyPad, robotPad, maxRobots, memo) ->
  return code.length if robots <= 0
  state = [code, robots]
  return memo[state] if state of memo
  ret = 0
  pos_i = if robots isnt maxRobots then 0 else 3
  pos_j = 2
  for ch in code
    if robots is maxRobots
      moves = generateMoves [pos_i, pos_j], ch, keyPad
      [pos_i, pos_j] = findPosition keyPad, ch
    else
      moves = generateMoves [pos_i, pos_j], ch, robotPad
      [pos_i, pos_j] = findPosition robotPad, ch
    ret += solve moves + "A", robots - 1, keyPad, robotPad, maxRobots, memo
  memo[state] = ret
  ret

content = fs.readFileSync("input.txt", 'utf8').trim()
maxRobots = 3
keyPad = ["789", "456", "123", " 0A"]
robotPad = [" ^A", "<v>"]
ret = 0
codes = content.split "\n"

for code in codes
  code = code.trim()
  continue if not code
  numericPart = 0
  for char in code
    if '0' <= char <= '9'
      numericPart = numericPart * 10 + parseInt(char)
  memo = {}
  sv = solve code, maxRobots, keyPad, robotPad, maxRobots, memo
  ret += sv * numericPart
console.log ret
