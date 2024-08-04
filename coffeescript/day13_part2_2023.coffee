fs = require 'fs'

class Mirror
  constructor: (@rows, @cols) ->

parseInput = (input) ->
  mirrors = []
  mirrorStr = []
  for line in input
    if line is ""
      mirrors.push parseMirror(mirrorStr)
      mirrorStr = []
    else
      mirrorStr.push line
  mirrors.push parseMirror(mirrorStr)
  mirrors

parseMirror = (mirrorStr) ->
  rows = new Array(mirrorStr.length)
  cols = new Array(mirrorStr[0].length)
  for y in [0...mirrorStr.length]
    for x in [0...mirrorStr[0].length]
      rows[y] = (rows[y] << 1) | (if mirrorStr[y][x] is '#' then 1 else 0)
      cols[x] = (cols[x] << 1) | (if mirrorStr[y][x] is '#' then 1 else 0)
  new Mirror(rows, cols)

getMirrorAxis = (lines) ->
  for i in [1...lines.length]
    isMirror = true
    for j in [0...Math.min(i, lines.length - i)]
      if lines[i - 1 - j] isnt lines[i + j]
        isMirror = false
        break
    return i if isMirror
  0

getMirrorAxisWithOneSmudge = (lines) ->
  for i in [1...lines.length]
    isMirror = true
    numSmudges = 0
    for j in [0...Math.min(i, lines.length - i)]
      if lines[i - 1 - j] isnt lines[i + j]
        if numSmudges > 0
          isMirror = false
          break
        else
          dif = lines[i - 1 - j] ^ lines[i + j]
          isOnlyOneSmudge = (dif & (dif - 1)) is 0
          if isOnlyOneSmudge
            numSmudges++
          else
            isMirror = false
            break
    return i if isMirror and numSmudges is 1
  0

solve = (input) ->
  mirrors = parseInput(input)
  res = 0
  for mirror in mirrors
    res += getMirrorAxisWithOneSmudge(mirror.cols)
    res += getMirrorAxisWithOneSmudge(mirror.rows) * 100
  res

readFile = (fileName) ->
  fs.readFileSync(fileName).toString().trim().split('\n')

input = readFile("input.txt")
console.log solve(input)