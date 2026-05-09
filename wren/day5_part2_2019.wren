import "io" for File

var content = File.read("input.txt").trim()
var program = []
for (s in content.split(",")) {
  program.add(Num.fromString(s))
}

var i = 0
var input = 5
var output = 0

while (true) {
  var instr = program[i]
  var opcode = instr % 100
  var modes = (instr / 100).truncate
  var param1Mode = modes % 10
  modes = (modes / 10).truncate
  var param2Mode = modes % 10

  if (opcode == 1) {
    var p1 = (param1Mode == 0) ? program[program[i+1]] : program[i+1]
    var p2 = (param2Mode == 0) ? program[program[i+2]] : program[i+2]
    var p3 = program[i+3]
    program[p3] = p1 + p2
    i = i + 4
  } else if (opcode == 2) {
    var p1 = (param1Mode == 0) ? program[program[i+1]] : program[i+1]
    var p2 = (param2Mode == 0) ? program[program[i+2]] : program[i+2]
    var p3 = program[i+3]
    program[p3] = p1 * p2
    i = i + 4
  } else if (opcode == 3) {
    var addr = program[i+1]
    program[addr] = input
    i = i + 2
  } else if (opcode == 4) {
    var val = (param1Mode == 0) ? program[program[i+1]] : program[i+1]
    output = val
    System.print(output)
    i = i + 2
  } else if (opcode == 5) {
    var p1 = (param1Mode == 0) ? program[program[i+1]] : program[i+1]
    var p2 = (param2Mode == 0) ? program[program[i+2]] : program[i+2]
    if (p1 != 0) {
      i = p2
    } else {
      i = i + 3
    }
  } else if (opcode == 6) {
    var p1 = (param1Mode == 0) ? program[program[i+1]] : program[i+1]
    var p2 = (param2Mode == 0) ? program[program[i+2]] : program[i+2]
    if (p1 == 0) {
      i = p2
    } else {
      i = i + 3
    }
  } else if (opcode == 7) {
    var p1 = (param1Mode == 0) ? program[program[i+1]] : program[i+1]
    var p2 = (param2Mode == 0) ? program[program[i+2]] : program[i+2]
    var p3 = program[i+3]
    if (p1 < p2) {
      program[p3] = 1
    } else {
      program[p3] = 0
    }
    i = i + 4
  } else if (opcode == 8) {
    var p1 = (param1Mode == 0) ? program[program[i+1]] : program[i+1]
    var p2 = (param2Mode == 0) ? program[program[i+2]] : program[i+2]
    var p3 = program[i+3]
    if (p1 == p2) {
      program[p3] = 1
    } else {
      program[p3] = 0
    }
    i = i + 4
  } else if (opcode == 99) {
    break
  } else {
    System.print("Invalid opcode")
    break
  }
}