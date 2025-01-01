
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')

grid = []
moves = ''
readingMap = true

for line in input
  if readingMap
    if line.includes '#'
      grid.push line
    else
      readingMap = false
      moves += line
  else
    moves += line

runes = grid.map (row) -> row.split ''

robotR = -1
robotC = -1

for r in [0...runes.length]
  for c in [0...runes[r].length]
    if runes[r][c] == '@'
      robotR = r
      robotC = c
      break
  break if robotR != -1

dirs =
  '^': [-1, 0]
  'v': [1, 0]
  '<': [0, -1]
  '>': [0, 1]

pushBoxes = (r, c, dr, dc) ->
  nr = r + dr
  nc = c + dc
  return false if runes[nr][nc] == '#'
  if runes[nr][nc] == 'O'
    return false unless pushBoxes(nr, nc, dr, dc)
  if runes[nr][nc] == '.'
    runes[nr][nc] = 'O'
    runes[r][c] = '.'
    return true
  return false

for move in moves
  d = dirs[move]
  nr = robotR + d[0]
  nc = robotC + d[1]
  continue if runes[nr][nc] == '#'
  if runes[nr][nc] == 'O'
    continue unless pushBoxes(nr, nc, d[0], d[1])
  if runes[nr][nc] == '.' or runes[nr][nc] == 'O'
    runes[robotR][robotC] = '.'
    runes[nr][nc] = '@'
    robotR = nr
    robotC = nc

sum = 0
for r in [0...runes.length]
  for c in [0...runes[r].length]
    if runes[r][c] == 'O'
      sum += r * 100 + c

console.log sum
