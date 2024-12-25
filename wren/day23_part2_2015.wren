
import "io" for File

var instructions = File.read("input.txt").trimEnd().split("\n")
var registers = {"a": 1, "b": 0}
var i = 0
while (i >= 0 && i < instructions.count) {
  var parts = instructions[i].split(" ")
  if (parts[0] == "hlf") {
    registers[parts[1]] = (registers[parts[1]] / 2).floor
  } else if (parts[0] == "tpl") {
    registers[parts[1]] = registers[parts[1]] * 3
  } else if (parts[0] == "inc") {
    registers[parts[1]] = registers[parts[1]] + 1
  } else if (parts[0] == "jmp") {
    var offset = Num.fromString(parts[1])
    i = i + offset - 1
  } else if (parts[0] == "jie") {
    if (registers[parts[1][0]] % 2 == 0) {
      var offset = Num.fromString(parts[2])
      i = i + offset - 1
    }
  } else if (parts[0] == "jio") {
    if (registers[parts[1][0]] == 1) {
      var offset = Num.fromString(parts[2])
      i = i + offset - 1
    }
  }
  i = i + 1
}
System.print(registers["b"])
