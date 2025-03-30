
import os
import strutils
import tables
import sets
import math # Needed for gcd in Part 2

# Define a type for coordinates for clarity
type Point = tuple[x, y: int]

# --- Greatest Common Divisor ---
# Standard Euclidean algorithm for GCD
proc gcd(a, b: int): int =
  var aa = abs(a)
  var bb = abs(b)
  while bb != 0:
    let temp = bb
    bb = aa mod bb
    aa = temp
  return aa

# --- Main Logic ---
proc main() =
  # 1. Read Input
  let inputLines = readFile("input.txt").strip.splitLines
  if inputLines.len == 0:
    echo "Input file is empty."
    return

  let gridHeight = inputLines.len
  let gridWidth = inputLines[0].len

  # Store antenna locations grouped by frequency
  var antennaLocations: Table[char, seq[Point]] = initTable[char, seq[Point]]()

  for y, line in inputLines:
    for x, char in line:
      if char != '.':
        if not antennaLocations.hasKey(char):
          antennaLocations[char] = newSeq[Point]()
        antennaLocations[char].add((x: x, y: y))

  # 2. Calculate Antinodes (Part 2 Logic)
  var antinodeSet: HashSet[Point] = initHashSet[Point]()

  for freq, antennas in antennaLocations.pairs:
    if antennas.len < 2: # Need at least two antennas for collinearity
      continue

    # Iterate through all unique pairs of antennas for this frequency
    for i in 0 ..< antennas.len:
      for j in i + 1 ..< antennas.len:
        let p1 = antennas[i]
        let p2 = antennas[j]

        let dx = p2.x - p1.x
        let dy = p2.y - p1.y

        # Calculate the smallest integer step vector along the line
        let commonDivisor = gcd(dx, dy)
        # Handle the case where p1 == p2 (shouldn't happen with j > i)
        # or if gcd somehow returns 0 (only if dx=0 and dy=0)
        if commonDivisor == 0: continue 
        
        let stepX = dx div commonDivisor
        let stepY = dy div commonDivisor

        # Extend the line in the positive direction (from p1 towards/past p2)
        var currentX = p1.x
        var currentY = p1.y
        while currentX >= 0 and currentX < gridWidth and currentY >= 0 and currentY < gridHeight:
          antinodeSet.incl((x: currentX, y: currentY))
          currentX += stepX
          currentY += stepY

        # Extend the line in the negative direction (from p1 away from p2)
        # Start one step *before* p1 to avoid adding p1 twice and check bounds first
        currentX = p1.x - stepX
        currentY = p1.y - stepY
        while currentX >= 0 and currentX < gridWidth and currentY >= 0 and currentY < gridHeight:
           antinodeSet.incl((x: currentX, y: currentY))
           currentX -= stepX
           currentY -= stepY


  # 3. Output Result
  echo antinodeSet.len

# --- Entry Point ---
when isMainModule:
  main()
