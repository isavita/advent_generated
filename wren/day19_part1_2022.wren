
import "io" for File

class BlueprintSolver {
  construct new(c_or_or, c_cl_or, c_ob_or, c_ob_c, c_ge_or, c_ge_ob, m_or) {
    _c_or_or = c_or_or
    _c_cl_or = c_cl_or
    _c_ob_or = c_ob_or
    _c_ob_c = c_ob_c
    _c_ge_or = c_ge_or
    _c_ge_ob = c_ge_ob
    _m_or = m_or
    _memo = {}
  }

  dfs(t, r1, r2, r3, o1, o2, o3) {
    if (t <= 1) return 0
    if (o1 > t * _m_or) o1 = t * _m_or
    if (o2 > t * _c_ob_c) o2 = t * _c_ob_c
    if (o3 > t * _c_ge_ob) o3 = t * _c_ge_ob

    var k = t + r1 * 32 + r2 * 1024 + r3 * 65536 + o1 * 4194304 + o2 * 1073741824 + o3 * 1099511627776
    if (_memo.containsKey(k)) return _memo[k]

    var r = 0
    if (o1 >= _c_ge_or && o3 >= _c_ge_ob) {
      r = (t - 1) + dfs(t - 1, r1, r2, r3, o1 + r1 - _c_ge_or, o2 + r2, o3 + r3 - _c_ge_ob)
    } else {
      r = dfs(t - 1, r1, r2, r3, o1 + r1, o2 + r2, o3 + r3)
      if (t >= 16 && r1 < _c_ob_or * 2 && o1 >= _c_or_or) {
        var nr = dfs(t - 1, r1 + 1, r2, r3, o1 + r1 - _c_or_or, o2 + r2, o3 + r3)
        if (nr > r) r = nr
      }
      if (t >= 8 && r2 < _c_ob_c && o1 >= _c_cl_or) {
        var nr = dfs(t - 1, r1, r2 + 1, r3, o1 + r1 - _c_cl_or, o2 + r2, o3 + r3)
        if (nr > r) r = nr
      }
      if (t >= 4 && r3 < _c_ge_ob && o1 >= _c_ob_or && o2 >= _c_ob_c) {
        var nr = dfs(t - 1, r1, r2, r3 + 1, o1 + r1 - _c_ob_or, o2 + r2 - _c_ob_c, o3 + r3)
        if (nr > r) r = nr
      }
    }
    _memo[k] = r
    return r
  }
}

class Main {
  static run() {
    var lines = File.read("input.txt").split("\n").map { |l| l.trim() }.where { |l| l.count > 0 }.toList
    var ans = 0
    for (line in lines) {
      var words = line.split(" ").where { |w| w != "" }.toList
      var id = Num.fromString(words[1].split(":")[0])
      var c_or_or = Num.fromString(words[6])
      var c_cl_or = Num.fromString(words[12])
      var c_ob_or = Num.fromString(words[18])
      var c_ob_c = Num.fromString(words[21])
      var c_ge_or = Num.fromString(words[27])
      var c_ge_ob = Num.fromString(words[30])

      var m_or = c_or_or
      if (c_cl_or > m_or) m_or = c_cl_or
      if (c_ob_or > m_or) m_or = c_ob_or
      if (c_ge_or > m_or) m_or = c_ge_or

      var solver = BlueprintSolver.new(c_or_or, c_cl_or, c_ob_or, c_ob_c, c_ge_or, c_ge_ob, m_or)
      var geodes = solver.dfs(24, 1, 0, 0, 0, 0, 0)
      ans = ans + id * geodes
    }
    System.print(ans)
  }
}

Main.run()
