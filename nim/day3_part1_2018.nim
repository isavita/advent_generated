import strutils, sequtils, tables

type Claim = object
  id: int
  x: int
  y: int
  width: int
  height: int

proc parseClaim(line: string): Claim =
  let parts = line.split(" ")
  let id = parseInt(parts[0][1..^1])
  let coords = parts[2][0..^2].split(",")
  let x = parseInt(coords[0])
  let y = parseInt(coords[1])
  let dims = parts[3].split("x")
  let width = parseInt(dims[0])
  let height = parseInt(dims[1])
  Claim(id: id, x: x, y: y, width: width, height: height)

proc countOverlaps(claims: seq[Claim]): int =
  var fabric = initTable[(int, int), int]()
  for claim in claims:
    for i in claim.x ..< claim.x + claim.width:
      for j in claim.y ..< claim.y + claim.height:
        if (i, j) notin fabric:
          fabric[(i, j)] = 0
        fabric[(i, j)] += 1

  var overlapCount = 0
  for count in fabric.values:
    if count > 1:
      overlapCount += 1
  overlapCount

when isMainModule:
  let file = open("input.txt")
  var claims: seq[Claim] = @[]
  for line in file.lines:
    claims.add(parseClaim(line))
  file.close()

  echo countOverlaps(claims)