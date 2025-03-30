
import strutils, tables, math, system

# Type alias for the node map for clarity
type NodeMap = Table[string, tuple[L, R: string]]

# Parses a single line like "AAA = (BBB, CCC)"
proc parseLine(line: string): (string, tuple[L, R: string]) =
  # Example: "AAA = (BBB, CCC)"
  # head = "AAA"
  # childrenStr = "BBB, CCC"
  let parts = line.split(" = ")
  let head = parts[0]
  # Slice to remove "(" and ")" -> "BBB, CCC"
  let childrenStr = parts[1][1 .. ^2]
  # Split by ", " -> ["BBB", "CCC"]
  let childrenParts = childrenStr.split(", ")
  result = (head, (childrenParts[0], childrenParts[1]))

# Parses the input lines into instructions string and the node map
proc parseInput(input: seq[string]): (string, NodeMap) =
  let instructions = input[0]
  var nodes = initTable[string, tuple[L, R: string]]()
  # Node definitions start from the 3rd line (index 2)
  for i in 2 ..< input.len:
    if input[i].len > 0: # Skip potential empty lines
      let (head, children) = parseLine(input[i])
      nodes[head] = children
  result = (instructions, nodes)

# Least Common Multiple using math.gcd
proc lcm(a, b: int): int =
  # Check for zero to avoid division by zero and match Python's behavior implicitly handled by gcd
  if a == 0 or b == 0:
    return 0
  # Use integer division `div`
  # Ensure intermediate multiplication doesn't overflow if possible, though int is likely 64-bit
  # Formula: lcm(a, b) = abs(a * b) / gcd(a, b)
  result = (a * b) div gcd(a, b)

# LCM of a sequence of integers
proc lcmList(nums: seq[int]): int =
  if nums.len == 0:
    return 0 # Match Python's behavior for empty list

  result = nums[0]
  for i in 1 ..< nums.len:
    result = lcm(result, nums[i])

# Solves the problem based on parsed input
proc solve(input: seq[string]): int =
  let (instructions, nodes) = parseInput(input)
  let instructionsLength = instructions.len

  if instructionsLength == 0: return 0 # Handle empty instructions

  # Find all starting nodes (ending with 'A')
  var starts: seq[string] = @[]
  for node in nodes.keys:
    if node.endsWith('A'):
      starts.add(node)

  if starts.len == 0: return 0 # Handle no starting nodes found

  # Calculate steps for each starting node to reach a node ending with 'Z'
  var steps = newSeq[int](starts.len)
  for i in 0 ..< starts.len:
    var element = starts[i]
    var currentSteps = 0
    # Loop until a node ending with 'Z' is found
    while not element.endsWith('Z'):
      # Get the current instruction, cycling through the instructions string
      let instruction = instructions[currentSteps mod instructionsLength]
      # Move to the next node based on the instruction
      if instruction == 'L':
        element = nodes[element].L
      else: # Assume 'R'
        element = nodes[element].R
      currentSteps += 1
      # Optional: Add a safeguard for potential infinite loops, although problem implies convergence
      # if currentSteps > some_very_large_number: raise newException(ValueError, "Cycle detected?")
    steps[i] = currentSteps # Store the number of steps for this path

  # Calculate the LCM of all the step counts
  result = lcmList(steps)

# Main entry point
when isMainModule:
  # Read all lines from the input file
  let inputData = readFile("input.txt").splitLines()
  # Solve the problem and print the result
  echo solve(inputData)
