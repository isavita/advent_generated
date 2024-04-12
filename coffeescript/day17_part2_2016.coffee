fs = require 'fs'
crypto = require 'crypto'

class Point
  constructor: (@x, @y, @path) ->

readPasscode = (filename) ->
  fs.readFileSync(filename, 'utf8').trim()

findLongestPathLength = (passcode) ->
  longest = 0
  queue = [new Point(0, 0, "")]
  while queue.length > 0
    point = queue.shift()
    if point.x == 3 and point.y == 3
      longest = Math.max(longest, point.path.length) if point.path.length > longest
      continue
    getOpenDoors(passcode, point.path).forEach (dir) ->
      nextPoint = new Point(point.x, point.y, point.path + dir)
      switch dir
        when "U" then nextPoint.y--
        when "D" then nextPoint.y++
        when "L" then nextPoint.x--
        when "R" then nextPoint.x++
      if nextPoint.x >= 0 and nextPoint.x < 4 and nextPoint.y >= 0 and nextPoint.y < 4
        queue.push(nextPoint)
  longest

getOpenDoors = (passcode, path) ->
  hash = md5Hash(passcode + path)
  doors = []
  doors.push "U" if hash[0] >= 'b' and hash[0] <= 'f'
  doors.push "D" if hash[1] >= 'b' and hash[1] <= 'f'
  doors.push "L" if hash[2] >= 'b' and hash[2] <= 'f'
  doors.push "R" if hash[3] >= 'b' and hash[3] <= 'f'
  doors

md5Hash = (input) ->
  hash = crypto.createHash('md5')
  hash.update(input)
  hash.digest('hex')

passcode = readPasscode("input.txt")
longestPathLength = findLongestPathLength(passcode)
console.log longestPathLength