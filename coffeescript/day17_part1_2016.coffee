fs = require 'fs'
crypto = require 'crypto'

class Point
  constructor: (@x, @y, @path) ->

findShortestPath = (passcode) ->
  queue = [new Point(0, 0, '')]
  while queue.length > 0
    point = queue.shift()
    return point.path if point.x == 3 && point.y == 3

    getOpenDoors(passcode, point.path).forEach (dir) ->
      nextPoint = new Point(point.x, point.y, point.path + dir)
      switch dir
        when 'U' then nextPoint.y--
        when 'D' then nextPoint.y++
        when 'L' then nextPoint.x--
        when 'R' then nextPoint.x++
      if nextPoint.x >= 0 && nextPoint.x < 4 && nextPoint.y >= 0 && nextPoint.y < 4
        queue.push nextPoint
  'No path found'

getOpenDoors = (passcode, path) ->
  hash = md5Hash(passcode + path)
  doors = []
  doors.push 'U' if hash[0] >= 'b' && hash[0] <= 'f'
  doors.push 'D' if hash[1] >= 'b' && hash[1] <= 'f'
  doors.push 'L' if hash[2] >= 'b' && hash[2] <= 'f'
  doors.push 'R' if hash[3] >= 'b' && hash[3] <= 'f'
  doors

md5Hash = (input) ->
  h = crypto.createHash('md5')
  h.update(input)
  h.digest('hex')

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  path = findShortestPath(data.trim())
  console.log path