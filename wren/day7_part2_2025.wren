
import "io" for File

class Problem {
  static solve() {
    var content = File.read("input.txt")
    var lines = content.split("\n")
    if (lines.count > 0 && lines[-1] == "") {
      lines.removeAt(-1)
    }
    var R = lines.count
    if (R == 0) {
      System.print(0)
      return
    }
    for (i in 0...R) {
      if (lines[i].count > 0 && lines[i][lines[i].count - 1] == "\r") {
        lines[i] = lines[i][0...-1]
      }
    }
    var C = lines[0].count
    var sr = -1
    var sc = -1
    for (r in 0...R) {
      var pos = lines[r].indexOf("S")
      if (pos != -1) {
        sr = r
        sc = pos
        break
      }
    }
    if (sr == -1) {
      System.print(0)
      return
    }
    var dp = List.filled(R, null)
    for (i in 0...R) {
      dp[i] = List.filled(C, 0)
    }
    dp[sr][sc] = 1
    var total = 0
    for (r in 0...R) {
      for (c in 0...C) {
        var v = dp[r][c]
        if (v == 0) continue
        var nr = r + 1
        if (nr == R) {
          total = total + v
          continue
        }
        var ch = lines[nr][c]
        if (ch == "^") {
          if (c - 1 < 0) {
            total = total + v
          } else {
            dp[nr][c - 1] = dp[nr][c - 1] + v
          }
          if (c + 1 >= C) {
            total = total + v
          } else {
            dp[nr][c + 1] = dp[nr][c + 1] + v
          }
        } else {
          dp[nr][c] = dp[nr][c] + v
        }
      }
    }
    System.print(total)
  }
}

Problem.solve()
