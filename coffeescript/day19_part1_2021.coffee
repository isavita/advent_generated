
fs = require 'fs'

# Define all 24 possible rotations
rotations = [
  (p) -> [ p[0],  p[1],  p[2]]
  (p) -> [ p[0], -p[2],  p[1]]
  (p) -> [ p[0], -p[1], -p[2]]
  (p) -> [ p[0],  p[2], -p[1]]
  (p) -> [-p[0], -p[1],  p[2]]
  (p) -> [-p[0],  p[2],  p[1]]
  (p) -> [-p[0],  p[1], -p[2]]
  (p) -> [-p[0], -p[2], -p[1]]
  (p) -> [ p[1], -p[0],  p[2]]
  (p) -> [ p[1], -p[2], -p[0]]
  (p) -> [ p[1],  p[0], -p[2]]
  (p) -> [ p[1],  p[2],  p[0]]
  (p) -> [-p[1],  p[0],  p[2]]
  (p) -> [-p[1], -p[2],  p[0]]
  (p) -> [-p[1], -p[0], -p[2]]
  (p) -> [-p[1],  p[2], -p[0]]
  (p) -> [ p[2],  p[1], -p[0]]
  (p) -> [ p[2],  p[0],  p[1]]
  (p) -> [ p[2], -p[1],  p[0]]
  (p) -> [ p[2], -p[0], -p[1]]
  (p) -> [-p[2],  p[1],  p[0]]
  (p) -> [-p[2], -p[0],  p[1]]
  (p) -> [-p[2], -p[1], -p[0]]
  (p) -> [-p[2],  p[0], -p[1]]
]

# Function to calculate the difference between two points
diff = (p1, p2) -> [p1[0] - p2[0], p1[1] - p2[1], p1[2] - p2[2]]

# Function to add two points
add = (p1, p2) -> [p1[0] + p2[0], p1[1] + p2[1], p1[2] + p2[2]]

# Function to check for overlaps between two scanners
findOverlap = (scanner1, scanner2) ->
  for rotation in rotations
    rotatedScanner2 = scanner2.map(rotation)
    counts = {}
    for p1 in scanner1
      for p2 in rotatedScanner2
        d = diff(p1, p2)
        key = d.join(',')
        counts[key] = (counts[key] || 0) + 1
        if counts[key] >= 12
          return {offset: d, rotatedBeacons: rotatedScanner2}
  return null

# Read input and parse scanners
input = fs.readFileSync('input.txt', 'utf8').trim()
scanners = input.split('\n\n').map((scanner) ->
  scanner.split('\n')[1..].map((line) -> line.split(',').map(Number))
)


# Initialize with the first scanner
beacons = new Set(scanners[0].map((p) -> p.join(',')))
scannerPositions = [[0, 0, 0]]
remainingScanners = scanners.slice(1)

# Main loop to find overlaps and merge scanners
while remainingScanners.length > 0
    foundMatch = false
    for i in [0...remainingScanners.length]
        scanner = remainingScanners[i]
        overlap = findOverlap(Array.from(beacons).map((s) -> s.split(',').map(Number)), scanner)

        if overlap
            foundMatch = true
            scannerPositions.push(overlap.offset)
            for beacon in overlap.rotatedBeacons
                beacons.add(add(beacon, overlap.offset).join(','))
            
            remainingScanners.splice(i, 1)
            break # Important: Break the inner loop after finding a match
    if not foundMatch
        console.error "No match found in this iteration. Check logic."
        break # Avoid infinite loop


# Output the result
console.log beacons.size
