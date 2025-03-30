
import strutils, tables, sequtils, algorithm, os

var memo = initTable[string, string]()

proc reverseString(s: string): string =
  result = newString(s.len)
  for i, c in s:
    result[s.high - i] = c

proc rotate(inputStr: string): string =
  let parts = inputStr.split('/')
  let size = parts.len
  var newParts = newSeq[string](size)
  for x in 0 ..< size:
    var newRow = ""
    for y in countdown(size - 1, 0):
      newRow.add(parts[y][x])
    newParts[x] = newRow
  result = newParts.join("/")

proc flip(inputStr: string): string =
  var parts = inputStr.split('/')
  for i in 0 ..< parts.len:
    parts[i] = reverseString(parts[i])
  result = parts.join("/")

proc enhance(inputStr: string, rules: Table[string, string]): string =
  if memo.contains(inputStr):
    return memo[inputStr]

  let original = inputStr
  var current = inputStr

  # Try original + 3 rotations
  for _ in 0 ..< 4:
    if rules.contains(current):
      result = rules[current]
      memo[original] = result
      return result
    current = rotate(current)

  # Flip original and try again + 3 rotations
  current = flip(original)
  for _ in 0 ..< 4:
    if rules.contains(current):
      result = rules[current]
      memo[original] = result
      return result
    current = rotate(current)

  # Should not happen based on problem description
  return ""

when isMainModule:
  var rules = initTable[string, string]()
  let fileContent = readFile("input.txt")
  for line in fileContent.strip.splitLines:
    let parts = line.split(" => ")
    if parts.len == 2:
      rules[parts[0]] = parts[1]

  var grid = @[".#.",
               "..#",
               "###"]

  for i in 0 ..< 18:
    let currentSize = grid.len
    var subSize: int
    var newSize: int

    if currentSize mod 2 == 0:
      subSize = 2
      newSize = (currentSize div 2) * 3
    else:
      subSize = 3
      newSize = (currentSize div 3) * 4

    var newGrid = newSeq[string](newSize)
    # Initialize rows to prevent index out of bounds during concatenation
    for r in 0 ..< newSize:
        newGrid[r] = ""

    let newSubSize = subSize + 1 # Size of the enhanced squares (3 or 4)

    for yBlock in 0 ..< (currentSize div subSize):
      for xBlock in 0 ..< (currentSize div subSize):
        # Extract square
        var square = newSeq[string](subSize)
        for dy in 0 ..< subSize:
          let y = yBlock * subSize + dy
          let x = xBlock * subSize
          square[dy] = grid[y][x ..< x + subSize]

        # Enhance square
        let squareStr = square.join("/")
        let newSquareStr = enhance(squareStr, rules)
        let newSquareParts = newSquareStr.split('/')

        # Place new square into newGrid
        let newGridYStart = yBlock * newSubSize
        for dy in 0 ..< newSubSize:
           newGrid[newGridYStart + dy].add(newSquareParts[dy])

    grid = newGrid
    # Optimization for iteration 5: Print the count directly if required by problem variant
    # if i == 4: # After 5 iterations (0-indexed)
    #   var count5 = 0
    #   for row in grid:
    #     for pixel in row:
    #       if pixel == '#':
    #         count5 += 1
    #   echo "Count after 5 iterations: ", count5


  var count = 0
  for row in grid:
    for pixel in row:
      if pixel == '#':
        count += 1

  echo count
