
fs = require 'fs'

parseInput = (input) ->
  scanners = []
  currentScanner = null
  for line in input.trim().split('\n')
    if line.startsWith('--- scanner')
      currentScanner = []
      scanners.push currentScanner
    else if line.trim() != ''
      [x, y, z] = line.split(',').map(Number)
      currentScanner.push [x, y, z]
  scanners

rotatePoint = (point, rotation) ->
  [x, y, z] = point
  switch rotation
    when 0 then [x, y, z]
    when 1 then [x, -z, y]
    when 2 then [x, -y, -z]
    when 3 then [x, z, -y]
    when 4 then [-x, -y, z]
    when 5 then [-x, -z, -y]
    when 6 then [-x, y, -z]
    when 7 then [-x, z, y]
    when 8 then [y, x, -z]
    when 9 then [y, -x, z]
    when 10 then [y, z, x]
    when 11 then [y, -z, -x]
    when 12 then [-y, x, z]
    when 13 then [-y, -x, -z]
    when 14 then [-y, -z, x]
    when 15 then [-y, z, -x]
    when 16 then [z, x, y]
    when 17 then [z, -x, -y]
    when 18 then [z, y, -x]
    when 19 then [z, -y, x]
    when 20 then [-z, x, -y]
    when 21 then [-z, -x, y]
    when 22 then [-z, y, x]
    when 23 then [-z, -y, -x]

allRotations = (points) ->
  rotations = []
  for r in [0..23]
    rotatedPoints = points.map (point) -> rotatePoint(point, r)
    rotations.push rotatedPoints
  rotations

findOverlap = (scanner1, scanner2) ->
  for rotation in [0..23]
    rotatedScanner2 = scanner2.map (point) -> rotatePoint(point, rotation)
    for p1 in scanner1
      for p2 in rotatedScanner2
        dx = p1[0] - p2[0]
        dy = p1[1] - p2[1]
        dz = p1[2] - p2[2]
        translatedScanner2 = rotatedScanner2.map (point) -> [point[0] + dx, point[1] + dy, point[2] + dz]
        overlapCount = 0
        for tp2 in translatedScanner2
          for p1_ in scanner1
            if tp2[0] == p1_[0] and tp2[1] == p1_[1] and tp2[2] == p1_[2]
              overlapCount += 1
              break
        if overlapCount >= 12
          return {
            rotation: rotation
            translation: [dx, dy, dz]
          }
  null

solve = (scanners) ->
  scannerPositions = [[0, 0, 0]]
  absoluteBeacons = new Set()
  for beacon in scanners[0]
    absoluteBeacons.add(beacon.join(','))
  
  unresolvedScanners = scanners[1..]
  
  while unresolvedScanners.length > 0
    for i in [0...unresolvedScanners.length]
      scanner = unresolvedScanners[i]
      overlap = findOverlap(Array.from(absoluteBeacons).map((s) -> s.split(',').map(Number)), scanner)
      if overlap
        [dx, dy, dz] = overlap.translation
        rotatedScanner = scanner.map (point) -> rotatePoint(point, overlap.rotation)
        scannerPositions.push [dx, dy, dz]
        for point in rotatedScanner
          absoluteBeacons.add([point[0] + dx, point[1] + dy, point[2] + dz].join(','))
        unresolvedScanners.splice(i, 1)
        break
  
  maxManhattan = 0
  for i in [0...scannerPositions.length]
    for j in [i+1...scannerPositions.length]
      [x1, y1, z1] = scannerPositions[i]
      [x2, y2, z2] = scannerPositions[j]
      manhattan = Math.abs(x1 - x2) + Math.abs(y1 - y2) + Math.abs(z1 - z2)
      maxManhattan = Math.max(maxManhattan, manhattan)
  
  console.log "Part 1: #{absoluteBeacons.size}"
  console.log "Part 2: #{maxManhattan}"

input = fs.readFileSync('input.txt', 'utf8')
scanners = parseInput(input)
solve(scanners)
