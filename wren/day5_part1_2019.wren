
import "io" for File

class Intcode {
  static run() {
    var content = File.read("input.txt").trim()
    var prog = content.split(",").map { |x| Num.fromString(x.trim()) }.toList

    var i = 0
    var diagnosticCode = 0
    var inputVal = 1

    var getValue = Fn.new { |mode, param|
      return (mode == 0) ? prog[param] : param
    }

    while (prog[i] != 99) {
      var instruction = prog[i]
      var opcode = instruction % 100
      var mode1 = ((instruction / 100).floor) % 10
      var mode2 = ((instruction / 1000).floor) % 10

      if (opcode == 1) {
        var p1 = prog[i+1]
        var p2 = prog[i+2]
        var p3 = prog[i+3]
        var val1 = getValue.call(mode1, p1)
        var val2 = getValue.call(mode2, p2)
        prog[p3] = val1 + val2
        i = i + 4
      } else if (opcode == 2) {
        var p1 = prog[i+1]
        var p2 = prog[i+2]
        var p3 = prog[i+3]
        var val1 = getValue.call(mode1, p1)
        var val2 = getValue.call(mode2, p2)
        prog[p3] = val1 * val2
        i = i + 4
      } else if (opcode == 3) {
        var p1 = prog[i+1]
        prog[p1] = inputVal
        i = i + 2
      } else if (opcode == 4) {
        var p1 = prog[i+1]
        diagnosticCode = getValue.call(mode1, p1)
        i = i + 2
      } else {
        break
      }
    }
    System.print(diagnosticCode)
  }
}

Intcode.run()
