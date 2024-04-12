fs = require 'fs'

class Point
  constructor: (@x, @y) ->

abs = (x) -> if x < 0 then -x else x

fs.readFile 'input.txt', 'utf8', (err, data) ->
  return console.error err if err

  lines = data.trim().split '\n'
  head = new Point 0, 0
  tail = new Point 0, 0
  visited = {}
  visited["#{tail.x},#{tail.y}"] = true

  for line in lines
    [dir, steps] = line.split ' '
    numSteps = parseInt steps, 10

    for i in [0...numSteps]
      switch dir
        when "R" then head.x++
        when "L" then head.x--
        when "U" then head.y++
        when "D" then head.y--

      if abs(head.x - tail.x) > 1 or abs(head.y - tail.y) > 1
        if head.x != tail.x and head.y != tail.y
          tail.x++ if head.x > tail.x
          tail.x-- if head.x < tail.x
          tail.y++ if head.y > tail.y
          tail.y-- if head.y < tail.y
        else
          tail.x++ if head.x > tail.x
          tail.x-- if head.x < tail.x
          tail.y++ if head.y > tail.y
          tail.y-- if head.y < tail.y

      visited["#{tail.x},#{tail.y}"] = true

  console.log Object.keys(visited).length