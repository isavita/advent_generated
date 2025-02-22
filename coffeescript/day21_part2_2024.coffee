fs = require 'fs'

find_position = (mat, ch) ->
  for i in [0...mat.length]
    for j in [0...mat[i].length]
      return [i, j] if mat[i][j] is ch
  [-1, -1]

ok = (mat, st, seq) ->
  [i, j] = st
  for ch in seq.split ''
    return false if i < 0 or i >= mat.length or j < 0 or j >= mat[i].length or mat[i][j] is ' '
    switch ch
      when '^' then i--
      when 'v' then i++
      when '<' then j--
      when '>' then j++
  true

generate_moves = (pos, obj, pad) ->
  [obj_i, obj_j] = find_position(pad, obj)
  [i, j] = pos
  res = ""
  res += "<".repeat(j - obj_j) if j > obj_j
  res += "^".repeat(i - obj_i) if i > obj_i
  res += "v".repeat(obj_i - i) if i < obj_i
  res += ">".repeat(obj_j - j) if j < obj_j
  unless ok(pad, pos, res)
    res = ""
    res += ">".repeat(obj_j - j) if j < obj_j
    res += "^".repeat(i - obj_i) if i > obj_i
    res += "v".repeat(obj_i - i) if i < obj_i
    res += "<".repeat(j - obj_j) if j > obj_j
  res

solve = (code, robots, key_pad, robot_pad, max_robots, memo = {}) ->
  key = code + "#" + robots + "#" + max_robots
  return memo[key] if key of memo
  return code.length if robots <= 0
  ret = 0
  [i, j] = if robots is max_robots then [3,2] else [0,2]
  for ch in code.split ''
    if robots is max_robots
      moves = generate_moves([i,j], ch, key_pad)
      [i, j] = find_position(key_pad, ch)
    else
      moves = generate_moves([i,j], ch, robot_pad)
      [i, j] = find_position(robot_pad, ch)
    ret += solve(moves + "A", robots - 1, key_pad, robot_pad, max_robots, memo)
  memo[key] = ret
  ret

content = fs.readFileSync("input.txt", "utf8").trim()
max_robots = 26
key_pad = [
  "789"
  "456"
  "123"
  " 0A"
]
robot_pad = [
  " ^A"
  "<v>"
]

total = 0
for code in content.split /\r?\n/
  code = code.trim()
  continue unless code
  num = 0
  for ch in code.split ''
    num = num * 10 + parseInt(ch) if /[0-9]/.test ch
  total += solve(code, max_robots, key_pad, robot_pad, max_robots) * num

console.log total