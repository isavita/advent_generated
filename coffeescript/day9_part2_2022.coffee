fs = require 'fs'

main = ->
  input = fs.readFileSync('input.txt', 'utf8')
  console.log visited(input, 10)

visited = (input, ropelen) ->
  rope = Array(ropelen).fill([0, 0])
  visited = new Set()
  
  for line in input.trim().split('\n')
    [b, n] = line.split(' ')
    d = dirFromByte b
    for i in [0...n]
      rope[0] = [rope[0][0] + d[0], rope[0][1] + d[1]]
      for j in [1...ropelen]
        rope[j] = next(rope[j - 1], rope[j])
      visited.add(rope[ropelen - 1].toString())
  
  visited.size

next = (head, tail) ->
  if Math.abs(head[0] - tail[0]) <= 1 and Math.abs(head[1] - tail[1]) <= 1
    tail
  else
    [tail[0] + sign(head[0] - tail[0]), tail[1] + sign(head[1] - tail[1])]

sign = (n) ->
  if n == 0 then 0 else if n < 0 then -1 else 1

dirFromByte = (b) ->
  switch b
    when 'N', 'U', '^' then [0, 1]
    when 'E', 'R', '>' then [1, 0]
    when 'S', 'D', 'v' then [0, -1]
    when 'W', 'L', '<' then [-1, 0]

main()