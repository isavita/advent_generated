import "io" for File

var main = Fn.new {
  var input = File.read("input.txt")
  var lines = input.split("\n")
  var instructions = []
  for (line in lines) {
    instructions.add(line.trim())
  }

  var a = 0
  var b = 0
  var pc = 0

  while (pc < instructions.count) {
    var instruction = instructions[pc]
    var parts = instruction.split(" ")

    if (parts[0] == "hlf") {
      if (parts[1] == "a") {
        a = a / 2
      } else {
        b = b / 2
      }
      pc = pc + 1
    } else if (parts[0] == "tpl") {
      if (parts[1] == "a") {
        a = a * 3
      } else {
        b = b * 3
      }
      pc = pc + 1
    } else if (parts[0] == "inc") {
      if (parts[1] == "a") {
        a = a + 1
      } else {
        b = b + 1
      }
      pc = pc + 1
    } else if (parts[0] == "jmp") {
      var offset = Num.fromString(parts[1])
      pc = pc + offset
    } else if (parts[0] == "jie") {
      var reg = parts[1][0]
      var offset = Num.fromString(parts[2])
      if ((reg == "a" && a % 2 == 0) || (reg == "b" && b % 2 == 0)) {
        pc = pc + offset
      } else {
        pc = pc + 1
      }
    } else if (parts[0] == "jio") {
      var reg = parts[1][0]
      var offset = Num.fromString(parts[2])
      if ((reg == "a" && a == 1) || (reg == "b" && b == 1)) {
        pc = pc + offset
      } else {
        pc = pc + 1
      }
    }
  }

  System.print(b)
}

main.call()