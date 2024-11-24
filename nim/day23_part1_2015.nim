
import std/[strutils, parseutils]

var 
  registers: array['a'..'b', int] = [0, 0]

proc main() =
  let instructions = readFile("input.txt").strip().splitLines()
  var i = 0

  while i < instructions.len:
    let parts = instructions[i].split()
    
    case parts[0]
    of "hlf":
      registers[parts[1][0]] = registers[parts[1][0]] div 2
    of "tpl":
      registers[parts[1][0]] *= 3
    of "inc":
      inc registers[parts[1][0]]
    of "jmp":
      i += parseInt(parts[1]) - 1
    of "jie":
      if registers[parts[1][0]] mod 2 == 0:
        i += parseInt(parts[2]) - 1
    of "jio":
      if registers[parts[1][0]] == 1:
        i += parseInt(parts[2]) - 1
    else:
      raise newException(ValueError, "Unknown instruction")
    
    inc i

  echo registers['b']

main()
