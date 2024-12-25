
import "io" for File

var instructions = File.read("input.txt").split("\n")
var registers = {"a": 0, "b": 0, "c": 0, "d": 0}

var getValue = Fn.new { |s|
  var val = Num.fromString(s)
  return val != null ? val : registers[s]
}

var i = 0
while (i < instructions.count) {
  var parts = instructions[i].split(" ")
  if (parts[0] == "cpy") {
    registers[parts[2]] = getValue.call(parts[1])
  } else if (parts[0] == "inc") {
    registers[parts[1]] = registers[parts[1]] + 1
  } else if (parts[0] == "dec") {
    registers[parts[1]] = registers[parts[1]] - 1
  } else if (parts[0] == "jnz") {
    if (getValue.call(parts[1]) != 0) {
      i = i + Num.fromString(parts[2])
      continue
    }
  }
  i = i + 1
}

System.print(registers["a"])
