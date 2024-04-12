fs = require 'fs'

class Pt3
  constructor: (@X, @Y, @Z) ->

  add: (p2) ->
    new Pt3 @X + p2.X, @Y + p2.Y, @Z + p2.Z

minFunc = (a, b) -> Math.min a, b
maxFunc = (a, b) -> Math.max a, b

do ->
  input = fs.readFileSync 'input.txt', 'utf8'
  lines = input.trim().split '\n'
  
  cubes = {}
  neighbors = [
    new Pt3(-1, 0, 0)
    new Pt3(1, 0, 0)
    new Pt3(0, -1, 0)
    new Pt3(0, 1, 0)
    new Pt3(0, 0, -1)
    new Pt3(0, 0, 1)
  ]
  min = new Pt3 Infinity, Infinity, Infinity
  max = new Pt3 -Infinity, -Infinity, -Infinity

  for line in lines
    continue unless line
    [x, y, z] = line.split(',').map (val) -> parseInt val
    cube = new Pt3 x, y, z
    cubes[cube.X + ',' + cube.Y + ',' + cube.Z] = true
    min = new Pt3 minFunc(min.X, cube.X), minFunc(min.Y, cube.Y), minFunc(min.Z, cube.Z)
    max = new Pt3 maxFunc(max.X, cube.X), maxFunc(max.Y, cube.Y), maxFunc(max.Z, cube.Z)

  min = min.add new Pt3(-1, -1, -1)
  max = max.add new Pt3(1, 1, 1)

  faces = 0
  q = [min]
  seen = {}
  seen[min.X + ',' + min.Y + ',' + min.Z] = true

  while q.length > 0
    curr = q.shift()
    for delta in neighbors
      next = curr.add delta
      nextKey = next.X + ',' + next.Y + ',' + next.Z
      if next.X < min.X or next.Y < min.Y or next.Z < min.Z or next.X > max.X or next.Y > max.Y or next.Z > max.Z
        continue
      if cubes[nextKey]
        faces++
      else if not seen[nextKey]
        seen[nextKey] = true
        q.push next

  console.log faces