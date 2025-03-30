
import strutils, sequtils, os, algorithm

proc solve(initialStacksInput: seq[string], steps: seq[string]): string =
  var numStacks = 0
  if initialStacksInput.len > 0 and initialStacksInput[^1].strip.len > 0:
      let numberLine = initialStacksInput[^1].strip.split(' ')
      if numberLine.len > 0:
          try:
              numStacks = parseInt(numberLine[^1])
          except ValueError:
              numStacks = (initialStacksInput[0].len + 1) div 4 # Fallback heuristic
      else:
          numStacks = (initialStacksInput[0].len + 1) div 4 # Fallback heuristic
  elif initialStacksInput.len > 0:
       numStacks = (initialStacksInput[0].len + 1) div 4 # Fallback heuristic


  var stacks = newSeq[seq[char]](numStacks)

  # Parse stacks bottom-up (iterate relevant lines in reverse)
  for lineIdx in countdown(initialStacksInput.len - 2, 0):
    let line = initialStacksInput[lineIdx]
    for i, c in line:
      if c >= 'A' and c <= 'Z':
        let stackIndex = (i - 1) div 4
        if stackIndex >= 0 and stackIndex < numStacks:
           stacks[stackIndex].add(c) # Top of stack is at the end of the seq

  # Process moves
  for step in steps:
    if step.len == 0: continue
    let parts = step.split(' ')
    if parts.len < 6: continue # Basic validation

    var n, frm, to: int
    try:
        n = parseInt(parts[1])
        frm = parseInt(parts[3]) - 1 # 0-based index
        to = parseInt(parts[5]) - 1 # 0-based index
    except ValueError:
        continue # Skip invalid lines

    let frmLen = stacks[frm].len
    if n > 0 and frm >= 0 and frm < numStacks and to >= 0 and to < numStacks and frmLen >= n:
        let startIndex = frmLen - n
        # Move crates maintaining order
        let cratesToMove = stacks[frm][startIndex ..< frmLen]
        stacks[to].add(cratesToMove)
        # Remove crates from source stack
        stacks[frm].setLen(startIndex)

  # Build result string from top of each stack
  var resultSeq: seq[char]
  for stack in stacks:
    if stack.len > 0:
      resultSeq.add(stack[^1]) # Get last element (top crate)

  result = resultSeq.join("")

when isMainModule:
  let fileContent = readFile("input.txt")
  let parts = fileContent.strip().split("\n\n")

  if parts.len == 2:
    let initialStacksInput = parts[0].splitLines()
    let steps = parts[1].strip().splitLines()
    echo solve(initialStacksInput, steps)
  else:
    echo "Invalid input format"

