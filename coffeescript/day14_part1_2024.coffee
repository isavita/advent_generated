
fs = require 'fs'

[width, height] = [101, 103]
robots = []

lines = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
for line in lines
  [pPart, vPart] = line.split(' ')
  [px, py] = pPart.substring(2).split(',').map(Number)
  [vx, vy] = vPart.substring(2).split(',').map(Number)
  robots.push [px, py, vx, vy]

for i in [0...100]
  for r in robots
    r[0] = (r[0] + r[2]) % width
    r[1] = (r[1] + r[3]) % height
    if r[0] < 0 then r[0] += width
    if r[1] < 0 then r[1] += height

q1 = 0
q2 = 0
q3 = 0
q4 = 0

for r in robots
  [x, y] = r
  continue if x is 50 or y is 51
  if x < 50 and y < 51 then q1++
  if x > 50 and y < 51 then q2++
  if x < 50 and y > 51 then q3++
  if x > 50 and y > 51 then q4++

console.log q1 * q2 * q3 * q4
