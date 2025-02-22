
fs = require 'fs'

# Function to parse a hailstone line
parseHailstone = (line) ->
  [posStr, velStr] = line.split ' @ '
  pos = posStr.split(', ').map (str) -> parseInt str, 10
  vel = velStr.split(', ').map (str) -> parseInt str, 10
  {px: pos[0], py: pos[1], pz: pos[2], vx: vel[0], vy: vel[1], vz: vel[2]}

# Function to find intersection of two hailstone paths in 2D (x, y)
intersect = (h1, h2) ->
  # Check if lines are parallel
  det = h1.vx * h2.vy - h2.vx * h1.vy
  if det is 0
    return null  # Parallel lines, no intersection

  # Calculate parameters t and u for the intersection point
  t = ((h2.px - h1.px) * h2.vy - (h2.py - h1.py) * h2.vx) / det
  u = -((h1.px - h2.px) * h1.vy - (h1.py - h2.py) * h1.vx) / det
  
  #Check that the intersection is in the future
  if t < 0 or u < 0 then return null

  # Intersection point
  ix = h1.px + h1.vx * t
  iy = h1.py + h1.vy * t

  {x: ix, y: iy, t: t, u: u}

# Part 1: Count intersections within the test area
solvePart1 = (hailstones, minCoord, maxCoord) ->
  count = 0
  for i in [0...hailstones.length]
    for j in [i+1...hailstones.length]
      intersection = intersect(hailstones[i], hailstones[j])
      if intersection? and intersection.x >= minCoord and intersection.x <= maxCoord and intersection.y >= minCoord and intersection.y <= maxCoord
        count++
  count

# Function to solve the linear equations using Gaussian elimination
gaussianElimination = (matrix) ->
    n = matrix.length
    
    # Forward elimination
    for i in [0...n]
        # Find pivot (largest absolute value) in current column
        maxRow = i
        for k in [i+1...n]
            if Math.abs(matrix[k][i]) > Math.abs(matrix[maxRow][i])
                maxRow = k

        # Swap rows if necessary
        [matrix[i], matrix[maxRow]] = [matrix[maxRow], matrix[i]]
        
        # Eliminate below current pivot
        for k in [i+1...n]
            factor = matrix[k][i] / matrix[i][i]
            for j in [i...n+1]
                if i is j
                    matrix[k][j] = 0
                else
                    matrix[k][j] -= factor * matrix[i][j]
                    
    # Back substitution
    x = (0 for _ in [0...n])
    for i in [n-1..0] by -1
        x[i] = matrix[i][n] / matrix[i][i]
        for k in [i-1..0] by -1
            matrix[k][n] -= matrix[k][i] * x[i]

    return x

# Part 2: Find rock's initial position and velocity
solvePart2 = (hailstones) ->
  # We will use the first 3 hailstones to solve for the rock's position and velocity
  # Formulate the system of linear equations
  # Equations are derived from:
  # px_rock + t_i * vx_rock = px_i + t_i * vx_i  (similarly for y and z)
  # Rearrange and eliminate t_i to create linear equations in terms of px_rock, py_rock, pz_rock, vx_rock, vy_rock, vz_rock
  
  h1 = hailstones[0]
  h2 = hailstones[1]
  h3 = hailstones[2]

  matrix = [
    [h2.vy - h1.vy, h1.vx - h2.vx, 0, h1.py - h2.py, h2.px - h1.px, 0, h2.px*h2.vy - h2.py*h2.vx - (h1.px*h1.vy - h1.py*h1.vx)],
    [h3.vy - h1.vy, h1.vx - h3.vx, 0, h1.py - h3.py, h3.px - h1.px, 0, h3.px*h3.vy - h3.py*h3.vx - (h1.px*h1.vy - h1.py*h1.vx)],
    [h2.vz - h1.vz, 0, h1.vx - h2.vx, h1.pz - h2.pz, 0, h2.px - h1.px, h2.px*h2.vz - h2.pz*h2.vx - (h1.px*h1.vz - h1.pz*h1.vx)],
    [h3.vz - h1.vz, 0, h1.vx - h3.vx, h1.pz - h3.pz, 0, h3.px - h1.px, h3.px*h3.vz - h3.pz*h3.vx - (h1.px*h1.vz - h1.pz*h1.vx)],
    [0, h2.vz - h1.vz, h1.vy - h2.vy, 0, h1.pz - h2.pz, h2.py - h1.py, h2.py*h2.vz - h2.pz*h2.vy - (h1.py*h1.vz - h1.pz*h1.vy)],
    [0, h3.vz - h1.vz, h1.vy - h3.vy, 0, h1.pz - h3.pz, h3.py - h1.py, h3.py*h3.vz - h3.pz*h3.vy - (h1.py*h1.vz - h1.pz*h1.vy)]
  ]

  # Solve the linear equations
  solution = gaussianElimination(matrix)
  # Round the solution to nearest integer
  rock =
    px: Math.round(solution[0])
    py: Math.round(solution[1])
    pz: Math.round(solution[2])
    vx: Math.round(solution[3])
    vy: Math.round(solution[4])
    vz: Math.round(solution[5])

  rock.px + rock.py + rock.pz



# Read input from file
inputFile = 'input.txt'
lines = fs.readFileSync(inputFile, 'utf8').trim().split '\n'
hailstones = lines.map parseHailstone

# Part 1
minCoord = 200000000000000
maxCoord = 400000000000000
part1Result = solvePart1(hailstones, minCoord, maxCoord)
console.log "Part 1: #{part1Result}"

# Part 2
part2Result = solvePart2 hailstones
console.log "Part 2: #{part2Result}"

