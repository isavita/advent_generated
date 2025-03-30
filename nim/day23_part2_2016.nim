
import strutils, tables, sequtils, os

proc readInput(filename: string): seq[string] =
  readFile(filename).strip.splitLines

proc isRegister(x: string): bool =
  x.len == 1 and x[0] in {'a', 'b', 'c', 'd'}

proc getValue(x: string, registers: Table[char, int]): int =
  if isRegister(x):
    registers[x[0]]
  else:
    parseInt(x)

proc executeProgram(instructions: var seq[string], registers: var Table[char, int]) =
  var i = 0
  while i < instructions.len:
    # Optimization check for multiplication pattern
    if i + 5 < instructions.len:
      let pattern = instructions[i..i+5]
      let p0 = pattern[0].splitWhitespace()
      let p1 = pattern[1].splitWhitespace()
      let p2 = pattern[2].splitWhitespace()
      let p3 = pattern[3].splitWhitespace()
      let p4 = pattern[4].splitWhitespace()
      let p5 = pattern[5].splitWhitespace()

      if p0.len == 3 and p0[0] == "cpy" and
         p1.len == 2 and p1[0] == "inc" and
         p2.len == 2 and p2[0] == "dec" and
         p3.len == 3 and p3[0] == "jnz" and
         p4.len == 2 and p4[0] == "dec" and
         p5.len == 3 and p5[0] == "jnz":

        let cpyX = p0[1]
        let cpyY = p0[2]
        let incA = p1[1]
        let decC = p2[1]
        let jnzC = p3[1]
        let jnzCOffset = p3[2]
        let decD = p4[1]
        let jnzD = p5[1]
        let jnzDOffset = p5[2]

        if isRegister(cpyY) and isRegister(incA) and isRegister(decC) and
           isRegister(jnzC) and isRegister(decD) and isRegister(jnzD) and
           incA == "a" and decC == cpyY and jnzC == cpyY and
           parseInt(jnzCOffset) == -2 and decD == "d" and jnzD == "d" and
           parseInt(jnzDOffset) == -5:

          registers['a'] += getValue(cpyX, registers) * registers['d']
          registers[cpyY[0]] = 0
          registers['d'] = 0
          i += 6
          continue

    let parts = instructions[i].splitWhitespace()
    let cmd = parts[0]

    case cmd
    of "tgl":
      let x = getValue(parts[1], registers)
      let targetIdx = i + x
      if targetIdx >= 0 and targetIdx < instructions.len:
        var targetParts = instructions[targetIdx].splitWhitespace()
        if targetParts.len == 2:
          if targetParts[0] == "inc":
            targetParts[0] = "dec"
          else:
            targetParts[0] = "inc"
        elif targetParts.len == 3:
          if targetParts[0] == "jnz":
            targetParts[0] = "cpy"
          else:
            targetParts[0] = "jnz"
        instructions[targetIdx] = targetParts.join(" ")
      i += 1
    of "cpy":
      if parts.len == 3:
        let x = parts[1]
        let y = parts[2]
        if isRegister(y):
          registers[y[0]] = getValue(x, registers)
      i += 1
    of "inc":
      if parts.len == 2:
        let x = parts[1]
        if isRegister(x):
          registers[x[0]] += 1
      i += 1
    of "dec":
      if parts.len == 2:
        let x = parts[1]
        if isRegister(x):
          registers[x[0]] -= 1
      i += 1
    of "jnz":
      if parts.len == 3:
        let x = parts[1]
        let y = parts[2]
        if getValue(x, registers) != 0:
          i += getValue(y, registers)
        else:
          i += 1
      else:
        i += 1 # Invalid jnz, skip
    else:
      i += 1 # Invalid or unknown instruction, skip

proc main() =
  var instructions = readInput("input.txt")
  var registers = {'a': 12, 'b': 0, 'c': 0, 'd': 0}.toTable
  executeProgram(instructions, registers)
  echo registers['a']

when isMainModule:
  main()
