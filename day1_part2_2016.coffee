
fs = require('fs')

class Position
  constructor: (@x, @y) ->

  toString: -> "#{@x},#{@y}"

firstRevisitedDistance = (instructions) ->
  pos = new Position(0, 0)
  visited = {}
  visited[pos.toString()] = true
  directions = [
    new Position(0, 1)
    new Position(1, 0)
    new Position(0, -1)
    new Position(-1, 0)
  ]
  dirIndex = 0 # Start facing North

  for instruction in instructions
    turn = instruction[0]
    blocks = parseInt(instruction.slice(1))

    if turn == 'R'
      dirIndex = (dirIndex + 1) % 4
    else
      dirIndex = (dirIndex - 1 + 4) % 4

    for i in [0...blocks]
      pos.x += directions[dirIndex].x
      pos.y += directions[dirIndex].y

      loc = pos.toString()
      if visited[loc]
        return Math.abs(pos.x) + Math.abs(pos.y)
      visited[loc] = true

  return -1 # No location visited twice

content = fs.readFileSync('input.txt', 'utf8')
instructions = content.split(', ').map (instruction) -> instruction.trim()
console.log(firstRevisitedDistance(instructions))
