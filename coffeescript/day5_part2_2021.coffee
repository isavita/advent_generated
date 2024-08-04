# Read input from file
fs = require 'fs'
input = fs.readFileSync 'input.txt', 'utf8'

# Parse input into line segments
segments = input.trim().split('\n').map (line) ->
  [start, end] = line.split ' -> '
  start = start.split(',').map (x) -> parseInt x
  end = end.split(',').map (x) -> parseInt x
  [start, end]

# Initialize a hash to store point counts
points = {}

# Function to mark a point on the diagram
markPoint = (x, y) ->
  key = "#{x},#{y}"
  points[key] = (points[key] or 0) + 1

# Function to draw a line on the diagram
drawLine = (start, end) ->
  [x1, y1] = start
  [x2, y2] = end

  # Calculate the direction of the line
  dx = Math.sign(x2 - x1)
  dy = Math.sign(y2 - y1)

  # Draw the line
  while x1 != x2 or y1 != y2
    markPoint x1, y1
    x1 += dx
    y1 += dy
  markPoint x1, y1

# Draw all lines on the diagram
for segment in segments
  drawLine segment...

# Count the number of points with at least two lines
count = 0
for key, value of points
  count++ if value >= 2

console.log count