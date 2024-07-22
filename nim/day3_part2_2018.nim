
import strutils, sequtils, os

type
  Claim = object
    id: int
    x: int
    y: int
    width: int
    height: int

proc readClaims(filename: string): seq[Claim] =
  var claims: seq[Claim] = @[]
  for line in lines(filename):
    let parts = line.split(" ")
    let id = parseInt(parts[0][1..^1])
    let coords = parts[2][0..^2].split(",")
    let x = parseInt(coords[0])
    let y = parseInt(coords[1])
    let dims = parts[3].split("x")
    let width = parseInt(dims[0])
    let height = parseInt(dims[1])
    claims.add(Claim(id: id, x: x, y: y, width: width, height: height))
  return claims

proc main() =
  let claims = readClaims("input.txt")
  var fabric = newSeq[int](1000000)
  
  for claim in claims:
    for y in claim.y ..< claim.y + claim.height:
      for x in claim.x ..< claim.x + claim.width:
        fabric[y * 1000 + x] += 1

  for claim in claims:
    var overlap = false
    for y in claim.y ..< claim.y + claim.height:
      for x in claim.x ..< claim.x + claim.width:
        if fabric[y * 1000 + x] > 1:
          overlap = true
          break
      if overlap: break
    if not overlap:
      echo claim.id
      return

main()
