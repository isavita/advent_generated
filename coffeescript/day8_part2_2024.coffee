
fs = require 'fs'

# Reads the antenna map from the input file.
readInput = (filename) ->
  fs.readFileSync(filename, 'utf8').trim().split('\n')

# Finds the positions of all antennas of a given frequency.
findAntennas = (grid, frequency) ->
  positions = []
  for row, y in grid
    for char, x in row
      positions.push({x, y}) if char is frequency
  positions

# Calculates the greatest common divisor (GCD) of two numbers.
gcd = (a, b) ->
  if b is 0 then a else gcd(b, a % b)

# Checks if a point is within the bounds of the grid.
isInBounds = (grid, x, y) ->
  x >= 0 and x < grid[0].length and y >= 0 and y < grid.length


# Determines if a point is an antinode (Part 2 logic - any point in line with 2+ antennas)
isAntinode = (grid, x, y, antennas) ->
  return false if antennas.length < 2  # Need at least two antennas

  count = 0
  for antenna1, i in antennas
    for antenna2, j in antennas
      continue if i is j # don't compare antenna with itself

      # Calculate vector from antenna1 to antenna2
      dx = antenna2.x - antenna1.x
      dy = antenna2.y - antenna1.y

      # Calculate vector from antenna1 to the point
      dx_point = x - antenna1.x
      dy_point = y - antenna1.y

      # Check for collinearity using cross-product (must be zero for collinearity)
      # And ensure the point is *not* between the two compared antennas
      if dx * dy_point is dy * dx_point
        if (dx isnt 0 or dy isnt 0) and ( (dx_point * dx >= 0 and dy_point * dy >=0) or (dx_point * dx <=0 and dy_point*dy <=0))
          if (dx_point*dx_point + dy_point*dy_point) > (dx * dx + dy * dy) or (dx_point is 0 and dy_point is 0)
            count++
            break # go to next antenna1

    break if count > 0  # found at least one set of collinear antennas
  count > 0

solve = (grid) ->
  frequencies = {}
  for row in grid
    for char in row
      frequencies[char] = true if /[0-9a-zA-Z]/.test(char)

  antinodes = {}
  for frequency of frequencies
    antennas = findAntennas(grid, frequency)

    # Iterate through all possible grid positions
    for y in [0...grid.length]
      for x in [0...grid[0].length]
          if isAntinode(grid, x, y, antennas)
            antinodes["#{x},#{y}"] = true  # Use a string key for uniqueness

  Object.keys(antinodes).length


# Main execution
grid = readInput('input.txt')
result = solve(grid)
console.log result
