fs = require 'fs'

class Star
  constructor: (@x, @y, @vX, @vY, @next = null) ->

toInt = (s) -> parseInt(s, 10)

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  lines = data.trim().split '\n'
  head = new Star 0, 0, 0, 0
  tail = head
  re = /position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>/

  for line in lines
    match = re.exec line
    continue unless match and match.length is 5
    star = new Star toInt(match[1]), toInt(match[2]), toInt(match[3]), toInt(match[4])
    tail.next = star
    tail = star

  smallestT = 0
  smallestArea = Infinity
  for t in [1...100000]
    maxX = maxY = -Infinity
    minX = minY = Infinity

    temp = head.next
    while temp
      x = temp.x + temp.vX * t
      maxX = Math.max maxX, x
      minX = Math.min minX, x
      y = temp.y + temp.vY * t
      maxY = Math.max maxY, y
      minY = Math.min minY, y
      temp = temp.next

    lenX = maxX - minX + 1
    lenY = maxY - minY + 1
    area = lenX * lenY

    if smallestArea > area
      smallestArea = area
      smallestT = t

  t = smallestT
  maxX = maxY = -Infinity
  minX = minY = Infinity

  temp = head.next
  while temp
    temp.x += temp.vX * t
    maxX = Math.max maxX, temp.x
    minX = Math.min minX, temp.x
    temp.y += temp.vY * t
    maxY = Math.max maxY, temp.y
    minY = Math.min minY, temp.y
    temp = temp.next

  mapper = Array(maxY - minY + 1).fill().map -> Array(maxX - minX + 1).fill false

  temp = head.next
  while temp
    mapper[temp.y - minY][temp.x - minX] = true
    temp = temp.next

  for row in mapper
    console.log row.map((cell) -> if cell then '#' else ' ').join('')