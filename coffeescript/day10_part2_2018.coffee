
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8')
lines = input.split('\n')

head = tail = {x: 0, y: 0, vX: 0, vY: 0, next: null}
re = /position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+), *(-?\d+)>/
for line in lines
  match = line.match re
  if match?
    star = {x: parseInt(match[1]), y: parseInt(match[2]), vX: parseInt(match[3]), vY: parseInt(match[4]), next: null}
    tail.next = star
    tail = star

smallestT = 0
smallestArea = 2**31
for t in [1...100000]
  maxX = maxY = minX = minY = 0
  temp = head.next
  while temp?
    x = temp.x + temp.vX*t
    if maxX < x then maxX = x else if minX > x then minX = x
    y = temp.y + temp.vY*t
    if maxY < y then maxY = y else if minY > y then minY = y
    temp = temp.next
  lenX = maxX - minX + 1
  lenY = maxY - minY + 1
  area = lenX * lenY
  if smallestArea > area then smallestArea = area; smallestT = t

console.log smallestT

t = smallestT
maxX = maxY = minX = minY = 0
temp = head.next
while temp?
  temp.x += temp.vX * t
  if maxX < temp.x then maxX = temp.x else if minX > temp.x then minX = temp.x
  temp.y += temp.vY * t
  if maxY < temp.y then maxY = temp.y else if minY > temp.y then minY = temp.y
  temp = temp.next

mapper = []
for y in [0...maxY-minY+1]
  row = []
  for x in [0...maxX-minX+1]
    row.push false
  mapper.push row

temp = head.next
while temp?
  mapper[temp.y - minY][temp.x - minX] = true
  temp = temp.next

console.log mapper
