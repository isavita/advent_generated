
import "io" for File

class Solver {
  static init(or_o, cl_o, ob_o, ob_c, go_o, go_i) {
    __or_o = or_o
    __cl_o = cl_o
    __ob_o = ob_o
    __ob_c = ob_c
    __go_o = go_o
    __go_i = go_i

    __mo = or_o
    if (cl_o > __mo) __mo = cl_o
    if (ob_o > __mo) __mo = ob_o
    if (go_o > __mo) __mo = go_o

    __oc = ob_c
    __gi = go_i
    __max_g = 0
    __memo = {}
  }

  static max_g { __max_g }

  static solve(t, r1, r2, r3, o1, o2, o3, g) {
    if (g + (t * (t - 1) / 2).floor <= __max_g) return
    if (g > __max_g) __max_g = g
    if (t <= 1) return

    if (r1 > __mo) r1 = __mo
    if (r2 > __oc) r2 = __oc
    if (r3 > __gi) r3 = __gi

    var v = t * __mo - r1 * (t - 1)
    if (o1 > v) o1 = v
    v = t * __oc - r2 * (t - 1)
    if (o2 > v) o2 = v
    v = t * __gi - r3 * (t - 1)
    if (o3 > v) o3 = v

    var k = t + r1 * 64 + r2 * 1024 + r3 * 32768 + o1 * 1048576 + o2 * 134217728 + o3 * 137438953472
    var val = __memo[k]
    if (val != null && val >= g) return
    __memo[k] = g

    var wo = o1 >= __go_o ? 0 : ((__go_o - o1 + r1 - 1) / r1).floor
    var wi = o3 >= __go_i ? 0 : (r3 == 0 ? 99 : ((__go_i - o3 + r3 - 1) / r3).floor)
    var w = (wo > wi ? wo : wi) + 1
    if (t - w > 0) {
      solve(t - w, r1, r2, r3, o1 + r1 * w - __go_o, o2 + r2 * w, o3 + r3 * w - __go_i, g + t - w)
    }

    if (r2 > 0) {
      wo = o1 >= __ob_o ? 0 : ((__ob_o - o1 + r1 - 1) / r1).floor
      var wc = o2 >= __ob_c ? 0 : ((__ob_c - o2 + r2 - 1) / r2).floor
      w = (wo > wc ? wo : wc) + 1
      if (t - w > 1 && r3 < __gi) {
        solve(t - w, r1, r2, r3 + 1, o1 + r1 * w - __ob_o, o2 + r2 * w - __ob_c, o3 + r3 * w, g)
      }
    }

    wo = o1 >= __cl_o ? 0 : ((__cl_o - o1 + r1 - 1) / r1).floor
    w = wo + 1
    if (t - w > 1 && r2 < __oc) {
      solve(t - w, r1, r2 + 1, r3, o1 + r1 * w - __cl_o, o2 + r2 * w, o3 + r3 * w, g)
    }

    wo = o1 >= __or_o ? 0 : ((__or_o - o1 + r1 - 1) / r1).floor
    w = wo + 1
    if (t - w > 1 && r1 < __mo) {
      solve(t - w, r1 + 1, r2, r3, o1 + r1 * w - __or_o, o2 + r2 * w, o3 + r3 * w, g)
    }
  }
}

var content = File.read("input.txt")
var lines = content.split("\n")

var getNumbers = Fn.new { |s|
  var res = []
  var cur = ""
  for (c in s) {
    if ("0123456789".contains(c)) {
      cur = cur + c
    } else {
      if (cur != "") {
        res.add(Num.fromString(cur))
        cur = ""
      }
    }
  }
  if (cur != "") {
    res.add(Num.fromString(cur))
  }
  return res
}

var ans = 1
var count = 0
for (line in lines) {
  if (line.trim() == "") continue
  if (count >= 3) break
  var nums = getNumbers.call(line)
  if (nums.count >= 7) {
    Solver.init(nums[1], nums[2], nums[3], nums[4], nums[5], nums[6])
    Solver.solve(32, 1, 0, 0, 0, 0, 0, 0)
    ans = ans * Solver.max_g
    count = count + 1
  }
}

System.print(ans)
