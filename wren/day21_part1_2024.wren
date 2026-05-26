
import "io" for File

class Solver {
  static rep(c, n) { n <= 0 ? "" : c * n }

  static pos(ch, isNum) {
    var idx = (isNum ? "789456123 0A" : " ^A<v>").indexOf(ch)
    return [(idx / 3).floor, idx % 3]
  }

  static ok(r, c, s, isNum) {
    for (i in 0...s.count) {
      var m = s[i]
      if (m == "^") r = r - 1
      if (m == "v") r = r + 1
      if (m == "<") c = c - 1
      if (m == ">") c = c + 1
      if (r < 0 || c < 0 || c > 2) return false
      if (isNum && (r > 3 || (r == 3 && c == 0))) return false
      if (!isNum && (r > 1 || (r == 0 && c == 0))) return false
    }
    return true
  }

  static mov(r1, c1, r2, c2, isNum) {
    var p1 = (c1 > c2 ? rep("<", c1 - c2) : "") +
             (r1 > r2 ? rep("^", r1 - r2) : "") +
             (r1 < r2 ? rep("v", r2 - r1) : "") +
             (c1 < c2 ? rep(">", c2 - c1) : "")
    if (ok(r1, c1, p1, isNum)) return p1
    return (c1 < c2 ? rep(">", c2 - c1) : "") +
           (r1 > r2 ? rep("^", r1 - r2) : "") +
           (r1 < r2 ? rep("v", r2 - r1) : "") +
           (c1 > c2 ? rep("<", c1 - c2) : "")
  }

  static solve(cd, rb, maxR, memo) {
    if (rb <= 0) return cd.count
    var key = cd + "," + rb.toString
    if (memo.containsKey(key)) return memo[key]
    var cr = (rb == maxR) ? 3 : 0
    var cc = 2
    var tot = 0
    for (i in 0...cd.count) {
      var target = pos(cd[i], rb == maxR)
      var trs = target[0]
      var tcs = target[1]
      var mv = mov(cr, cc, trs, tcs, rb == maxR)
      tot = tot + solve(mv + "A", rb - 1, maxR, memo)
      cr = trs
      cc = tcs
    }
    memo[key] = tot
    return tot
  }
}

var maxR = 3
var memo = {}
var ans = 0
var lines = File.read("input.txt").split("\n")
for (line in lines) {
  var c = ""
  for (ch in line) {
    if (ch != " " && ch != "\r" && ch != "\n" && ch != "\t") c = c + ch
  }
  if (c == "") continue
  var n_str = ""
  for (i in 0...c.count) {
    var ch = c[i]
    if ("0123456789".contains(ch)) n_str = n_str + ch
  }
  ans = ans + Solver.solve(c, maxR, maxR, memo) * Num.fromString(n_str)
}
System.print(ans)
