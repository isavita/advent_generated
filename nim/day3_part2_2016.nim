
import strutils, sequtils, os

# Function to check if three sides can form a triangle
proc isValidTriangle(a: int, b: int, c: int): bool =
  return (a + b > c) and (a + c > b) and (b + c > a)

# Function to read triangles from the input file and count valid ones
proc countValidTriangles(filename: string): int =
  let lines = readFile(filename).splitLines()
  var validCount = 0

  # Read triangles in groups of three vertically
  for col in 0 ..< 3:
    for row in 0 ..< (lines.len div 3):
      let idx1 = row * 3
      let idx2 = row * 3 + 1
      let idx3 = row * 3 + 2
      
      if idx1 < lines.len and idx2 < lines.len and idx3 < lines.len:
        let sides = [
          parseInt(lines[idx1].splitWhitespace()[col]),
          parseInt(lines[idx2].splitWhitespace()[col]),
          parseInt(lines[idx3].splitWhitespace()[col])
        ]
        
        if isValidTriangle(sides[0], sides[1], sides[2]):
          validCount += 1

  return validCount

# Main program execution
let filename = "input.txt"
let result = countValidTriangles(filename)
echo result
