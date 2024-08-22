import strutils, sequtils, os

type
  Screen = array[0..5, array[0..49, bool]]

proc rect(screen: var Screen, a, b: int) =
  for y in 0..<b:
    for x in 0..<a:
      screen[y][x] = true

proc rotateRow(screen: var Screen, y, b: int) =
  let row = screen[y]
  var newRow: array[0..49, bool]
  for x in 0..<50:
    newRow[(x + b) mod 50] = row[x]
  screen[y] = newRow

proc rotateColumn(screen: var Screen, x, b: int) =
  var newCol: array[0..5, bool]
  for y in 0..<6:
    newCol[(y + b) mod 6] = screen[y][x]
  for y in 0..<6:
    screen[y][x] = newCol[y]

proc countLitPixels(screen: Screen): int =
  for row in screen:
    for pixel in row:
      if pixel:
        result.inc

proc main() =
  var screen: Screen
  let file = open("input.txt")
  for line in file.lines:
    if line.startsWith("rect"):
      let parts = line[5..^1].split('x')
      let a = parseInt(parts[0])
      let b = parseInt(parts[1])
      rect(screen, a, b)
    elif line.startsWith("rotate row"):
      let parts = line.split('=')
      let y = parseInt(parts[1].split(" by ")[0])
      let b = parseInt(parts[1].split(" by ")[1])
      rotateRow(screen, y, b)
    elif line.startsWith("rotate column"):
      let parts = line.split('=')
      let x = parseInt(parts[1].split(" by ")[0])
      let b = parseInt(parts[1].split(" by ")[1])
      rotateColumn(screen, x, b)
  echo "Lit pixels: ", countLitPixels(screen)

when isMainModule:
  main()