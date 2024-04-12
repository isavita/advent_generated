fs = require 'fs'

class Ship
  constructor: ->
    @x = 0
    @y = 0
    @facing = 0

  processInstruction: (action, value) ->
    switch action
      when 'N' then @y += value
      when 'S' then @y -= value
      when 'E' then @x += value
      when 'W' then @x -= value
      when 'L' then @facing = (@facing - value + 360) % 360
      when 'R' then @facing = (@facing + value) % 360
      when 'F'
        switch @facing
          when 0 then @x += value
          when 90 then @y -= value
          when 180 then @x -= value
          when 270 then @y += value

abs = (x) -> if x < 0 then -x else x

data = fs.readFileSync 'input.txt', 'utf8'
ship = new Ship()
for line in data.trim().split '\n'
  action = line[0]
  value = parseInt line[1..]
  ship.processInstruction action, value

manhattanDistance = abs(ship.x) + abs(ship.y)
console.log manhattanDistance