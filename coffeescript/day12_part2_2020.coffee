fs = require 'fs'

class Ship
  constructor: ->
    @x = 0
    @y = 0
    @waypointX = 10
    @waypointY = 1

  processInstruction: (action, value) ->
    switch action
      when 'N' then @waypointY += value
      when 'S' then @waypointY -= value
      when 'E' then @waypointX += value
      when 'W' then @waypointX -= value
      when 'L' then @rotateWaypoint -value
      when 'R' then @rotateWaypoint value
      when 'F'
        @x += @waypointX * value
        @y += @waypointY * value

  rotateWaypoint: (degrees) ->
    degrees = (degrees + 360) % 360
    switch degrees
      when 90, -270
        [@waypointX, @waypointY] = [@waypointY, -@waypointX]
      when 180, -180
        [@waypointX, @waypointY] = [-@waypointX, -@waypointY]
      when 270, -90
        [@waypointX, @waypointY] = [-@waypointY, @waypointX]

abs = (x) -> if x < 0 then -x else x

fs.readFile 'input.txt', 'utf8', (err, data) ->
  return console.log "Error reading file:", err if err
  lines = data.trim().split '\n'
  ship = new Ship()
  for line in lines
    action = line[0]
    value = parseInt line[1..]  # Fixed this line
    ship.processInstruction action, value

  manhattanDistance = abs(ship.x) + abs(ship.y)
  console.log manhattanDistance