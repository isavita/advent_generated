
import std/[strutils, os, parseutils, algorithm]

type Nanobot = object
  x, y, z, radius: int

proc parseNanobot(line: string): Nanobot =
  var s = line
  s = s.replace("pos=<", "")
  let parts = s.split(">, r=")
  let coords = parts[0].split(",")
  result.x = parseInt(coords[0])
  result.y = parseInt(coords[1])
  result.z = parseInt(coords[2])
  result.radius = parseInt(parts[1])

proc manhattan(a, b: Nanobot): int =
  abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z)

proc main =
  let lines = readFile("input.txt").splitLines()
  var bots: seq[Nanobot] = newSeq[Nanobot](lines.len)
  for i, l in lines:
    bots[i] = parseNanobot(l)

  var strongest = bots[0]
  for b in bots:
    if b.radius > strongest.radius:
      strongest = b

  var count = 0
  for b in bots:
    if manhattan(b, strongest) <= strongest.radius:
      inc count

  echo count

when isMainModule:
  main()
