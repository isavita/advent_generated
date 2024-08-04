fs = require 'fs'

class Mirror
  constructor: (@rows, @cols) ->

parseInput = (input) ->
  mirrors = []
  mirrorStr = []

  for line in input
    if line == ""
      mirrors.push parseMirror(mirrorStr)
      mirrorStr = []
    else
      mirrorStr.push line

  mirrors.push parseMirror(mirrorStr)
  mirrors

parseMirror = (mirrorStr) ->
  rows = new Array(mirrorStr.length).fill(0)
  cols = new Array(mirrorStr[0].length).fill(0)

  for y in [0...mirrorStr.length]
    for x in [0...mirrorStr[0].length]
      rows[y] <<= 1
      cols[x] <<= 1
      if mirrorStr[y][x] == '#'
        rows[y]++
        cols[x]++

  new Mirror(rows, cols)

getMirrorAxis = (lines) ->
  for i in [1...lines.length]
    isMirror = true
    for j in [0...Math.min(i, lines.length - i)]
      if lines[i - 1 - j] != lines[i + j]
        isMirror = false
        break
    if isMirror
      return i
  0

getMirrorAxisWithOneSmudge = (lines) ->
  for i in [1...lines.length]
    isMirror = true
    numSmudges = 0
    for j in [0...Math.min(i, lines.length - i)]
      if lines[i - 1 - j] != lines[i + j]
        if numSmudges > 0
          isMirror = false
          break
        else
          dif = lines[i - 1 - j] ^ lines[i + j]
          isOnlyOneSmudge = (dif & (dif - 1)) == 0
          if isOnlyOneSmudge
            numSmudges++
          else
            isMirror = false
            break
    if isMirror and numSmudges == 1
      return i
  0

solve = (input) ->
  mirrors = parseInput(input)
  res = 0
  for mirror in mirrors
    res += getMirrorAxis(mirror.cols)
    res += getMirrorAxis(mirror.rows) * 100
  res

input = fs.readFileSync('input.txt', 'utf8').trim().split('\n')
console.log solve(input)