fs = require 'fs'

class Point
  constructor: (@x, @y) ->

  foldX: (value) ->
    @x = 2 * value - @x if @x > value

  foldY: (value) ->
    @y = 2 * value - @y if @y > value

  toString: ->
    "#{@x},#{@y}"

data = fs.readFileSync 'input.txt', 'utf8'
lines = data.trim().split '\n'

points = {}
folds = []
readingPoints = true

for line in lines
  if line == ''
    readingPoints = false
    continue
  if readingPoints
    [x, y] = line.split ','
    point = new Point parseInt(x), parseInt(y)
    points[point.toString()] = true
  else
    folds.push line

fold = folds[0].split(' ')[2]  # "fold along x=5"
[axis, value] = fold.split '='
value = parseInt value

newPoints = {}
for key, _ of points
  [x, y] = key.split ','
  point = new Point parseInt(x), parseInt(y)
  if axis == 'x'
    point.foldX value
  else
    point.foldY value
  newPoints[point.toString()] = true

console.log Object.keys(newPoints).length