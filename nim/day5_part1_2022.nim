
import os, strutils, sequtils, algorithm, strscans

proc solve(st: seq[seq[char]], steps: seq[string]): string =
  var stacks = st # Create a mutable copy
  for stack in stacks.mitems:
    stack.reverse() # Reverse so the top element is at the end

  for step in steps:
    var count, fromStack, toStack: int
    if scanf(step, "move $i from $i to $i", count, fromStack, toStack):
      fromStack -= 1 # Adjust to 0-based index
      toStack -= 1   # Adjust to 0-based index
      for _ in 1..count:
        if stacks[fromStack].len > 0:
          stacks[toStack].add(stacks[fromStack].pop())

  result = ""
  for stack in stacks:
    if stack.len > 0:
      result.add(stack[^1])

proc main() =
  let fileContent = readFile("input.txt")
  let parts = fileContent.strip(trailing = true).split("\n\n")
  let initialConfigLines = parts[0].splitLines()
  let steps = parts[1].splitLines()

  let numberLine = initialConfigLines[^1]
  # Determine number of stacks by parsing the last number on the number line
  let numStacks = numberLine.strip().split(Whitespace)[^1].parseInt()

  var initialStacks = newSeqWith(numStacks, newSeq[char]())

  # Parse initial stack config (iterate lines excluding the number line)
  for i in 0 ..< initialConfigLines.high:
    let line = initialConfigLines[i]
    # Iterate through character positions where crates could be ([X] at 1, 5, 9, ...)
    for stackIdx in 0 ..< numStacks:
        let charIdx = 1 + stackIdx * 4
        if charIdx < line.len and line[charIdx] >= 'A' and line[charIdx] <= 'Z':
            initialStacks[stackIdx].add(line[charIdx]) # Add crate to the bottom first

  let answer = solve(initialStacks, steps)
  echo answer

when isMainModule:
  main()
