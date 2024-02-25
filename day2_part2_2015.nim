import strutils, sequtils, algorithm

# Function to calculate the ribbon needed for a single present
proc ribbonNeededForPresent(dimensions: string): int =
  var dims = dimensions.split('x').map(proc(x: string): int = parseInt(x))
  dims.sort()

  # Calculate the smallest perimeter and the bow
  let smallestPerimeter = 2 * dims[0] + 2 * dims[1]
  let bow = dims[0] * dims[1] * dims[2]

  return smallestPerimeter + bow

proc totalRibbonNeeded(filename: string): int =
  var totalRibbon = 0
  for line in lines(filename):
    totalRibbon += ribbonNeededForPresent(line)
  return totalRibbon

let filename = "input.txt"
let totalRibbon = totalRibbonNeeded(filename)
echo totalRibbon
