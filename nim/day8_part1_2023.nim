
import strutils, tables, system

const
  InputFile = "input.txt"
  StartNode = "AAA"
  ElemToMatch = "ZZZ"

proc main() =
  let content = readFile(InputFile)
  var lines = content.strip().splitLines()

  let instructions = lines[0]
  var desertMap = initTable[string, tuple[left: string, right: string]]()

  for i in 2..high(lines): # Use high() for last index
      let line = lines[i]
      if line.len >= 16: # "XXX = (YYY, ZZZ)" needs at least 16 chars
          # Optimized parsing using string slicing instead of regex
          let key = line[0..2]
          let left = line[7..9]
          let right = line[12..14]
          desertMap[key] = (left: left, right: right)

  var current = StartNode
  var steps = 0
  let instructionsLen = instructions.len # Cache length for slight optimization

  while current != ElemToMatch:
      # Cycle through instructions using modulo operator
      let direction = instructions[steps mod instructionsLen]

      if direction == 'L':
          current = desertMap[current].left
      elif direction == 'R':
          current = desertMap[current].right
      # Assume valid instructions 'L' or 'R'

      steps += 1

  echo steps

main()
