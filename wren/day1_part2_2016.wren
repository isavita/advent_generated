
import "io" for File

var solve = Fn.new {
  var content = File.read("input.txt")
  var dx = [0, 1, 0, -1]
  var dy = [1, 0, -1, 0]
  var x = 0
  var y = 0
  var dir = 0
  var visited = {"0,0": true}

  for (rawInstr in content.split(",")) {
    var instr = rawInstr.trim()
    if (instr.count > 0) {
      var turn = instr[0]
      var steps = Num.fromString(instr[1..-1])
      dir = (turn == "R") ? (dir + 1) % 4 : (dir + 3) % 4
      var stepX = dx[dir]
      var stepY = dy[dir]
      for (j in 0...steps) {
        x = x + stepX
        y = y + stepY
        var key = "%(x),%(y)"
        if (visited.containsKey(key)) {
          System.print(x.abs + y.abs)
          return
        }
        visited[key] = true
      }
    }
  }
}

solve.call()
