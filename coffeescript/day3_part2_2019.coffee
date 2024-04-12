fs = require 'fs'

class Point
  constructor: (@x, @y) ->

  toString: ->
    "#{@x},#{@y}"

getPointsWithSteps = (path) ->
  points = {}
  current = new Point 0, 0
  steps = 0
  for move in path.split ','
    dir = move[0]
    dist = parseInt move[1..]
    for i in [1..dist]
      steps++
      switch dir
        when 'U' then current.y++
        when 'D' then current.y--
        when 'L' then current.x--
        when 'R' then current.x++
      points[current.toString()] ?= steps
  points

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log err
    return
  lines = data.trim().split '\n'
  wire1 = getPointsWithSteps lines[0]
  wire2 = getPointsWithSteps lines[1]

  minSteps = Number.MAX_SAFE_INTEGER
  for p, steps1 of wire1
    if wire2[p]
      totalSteps = steps1 + wire2[p]
      if totalSteps < minSteps
        minSteps = totalSteps

  console.log minSteps