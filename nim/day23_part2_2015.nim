import strutils

type Instruction = object
  op: string
  reg: char
  offset: int

proc parseInstruction(line: string): Instruction =
  let parts = line.split(' ')
  case parts[0]
  of "hlf", "tpl", "inc":
    Instruction(op: parts[0], reg: parts[1][0], offset: 0)
  of "jmp":
    Instruction(op: parts[0], reg: ' ', offset: parseInt(parts[1]))
  of "jie", "jio":
    Instruction(op: parts[0], reg: parts[1][0], offset: parseInt(parts[2]))
  else:
    raise newException(IOError, "Unknown instruction")

proc executeProgram(instructions: seq[Instruction]): int =
  var a = 1
  var b = 0
  var ip = 0

  while ip < instructions.len:
    let instr = instructions[ip]
    case instr.op
    of "hlf":
      if instr.reg == 'a':
        a = a div 2
      else:
        b = b div 2
      inc ip
    of "tpl":
      if instr.reg == 'a':
        a = a * 3
      else:
        b = b * 3
      inc ip
    of "inc":
      if instr.reg == 'a':
        inc a
      else:
        inc b
      inc ip
    of "jmp":
      ip += instr.offset
    of "jie":
      if (instr.reg == 'a' and a mod 2 == 0) or (instr.reg == 'b' and b mod 2 == 0):
        ip += instr.offset
      else:
        inc ip
    of "jio":
      if (instr.reg == 'a' and a == 1) or (instr.reg == 'b' and b == 1):
        ip += instr.offset
      else:
        inc ip
    else:
      raise newException(IOError, "Unknown instruction")

  return b

when isMainModule:
  let file = open("input.txt")
  var instructions: seq[Instruction] = @[]
  for line in file.lines:
    instructions.add(parseInstruction(line))
  file.close()

  let result = executeProgram(instructions)
  echo result