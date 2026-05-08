
import "io" for File

var runProgram = Fn.new { |program|
  var ip = 0
  while (ip < program.count) {
    var opcode = program[ip]
    if (opcode == 1) {
      var a = program[ip+1]
      var b = program[ip+2]
      var c = program[ip+3]
      program[c] = program[a] + program[b]
      ip = ip + 4
    } else if (opcode == 2) {
      var a = program[ip+1]
      var b = program[ip+2]
      var c = program[ip+3]
      program[c] = program[a] * program[b]
      ip = ip + 4
    } else if (opcode == 99) {
      break
    } else {
      Fiber.abort("Unknown opcode")
    }
  }
  return program[0]
}

var input = File.read("input.txt").trim()
var program = input.split(",").map { |s| Num.fromString(s) }.toList
program[1] = 12
program[2] = 2
System.print(runProgram.call(program))
