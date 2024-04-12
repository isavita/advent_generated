fs = require 'fs'

class Claim
  constructor: (@id, @left, @top, @width, @height) ->

parseClaim = (s) ->
  parts = /^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$/.exec(s)
  return new Claim parseInt(parts[1]), parseInt(parts[2]), parseInt(parts[3]), parseInt(parts[4]), parseInt(parts[5])

readClaims = (filename) ->
  data = fs.readFileSync filename, 'utf8'
  claims = []
  for line in data.trim().split('\n')
    claim = parseClaim line
    claims.push claim
  return claims

countOverlappingInches = (claims) ->
  fabric = {}
  for claim in claims
    for i in [claim.left...claim.left + claim.width]
      for j in [claim.top...claim.top + claim.height]
        coord = "#{i},#{j}"
        fabric[coord] = (fabric[coord] or 0) + 1

  overlapping = 0
  for count of fabric
    overlapping++ if fabric[count] > 1
  return overlapping

try
  claims = readClaims "input.txt"
  overlapping = countOverlappingInches claims
  console.log overlapping
catch error
  console.error error