
import "io" for File

class Monkey {
  static init() {
    __val = {}
    __hasVal = {}
    __left = {}
    __right = {}
    __op = {}
  }

  static solve(n) {
    if (__hasVal[n] == true) return __val[n]
    if (__left[n] && __right[n]) {
      var l = solve(__left[n])
      var r = solve(__right[n])
      if (l != null && r != null) {
        var o = __op[n]
        if (o == "+") return l + r
        if (o == "-") return l - r
        if (o == "*") return l * r
        if (o == "/") return (l / r).truncate
        if (o == "==") return (l == r) ? 0 : 1
      }
    }
    return null
  }

  static expect(n, t) {
    if (n == "humn") return t
    if (__left[n] && __right[n]) {
      var l = solve(__left[n])
      var r = solve(__right[n])
      var o = __op[n]
      if (l == null) {
        if (o == "+") return expect(__left[n], t - r)
        if (o == "-") return expect(__left[n], t + r)
        if (o == "*") return expect(__left[n], (t / r).truncate)
        if (o == "/") return expect(__left[n], t * r)
        if (o == "==") return expect(__left[n], r)
      }
      if (r == null) {
        if (o == "+") return expect(__right[n], t - l)
        if (o == "-") return expect(__right[n], l - t)
        if (o == "*") return expect(__right[n], (t / l).truncate)
        if (o == "/") return expect(__right[n], (l / t).truncate)
        if (o == "==") return expect(__right[n], l)
      }
    }
    return null
  }

  static run() {
    init()
    var content = File.read("input.txt")
    var lines = []
    var cur = ""
    for (c in content) {
      if (c == "\n") {
        lines.add(cur)
        cur = ""
      } else if (c != "\r") {
        cur = cur + c
      }
    }
    if (cur != "") lines.add(cur)

    for (line in lines) {
      if (line == "") continue
      var parts = line.split(": ")
      var name = parts[0]
      var rest = parts[1]
      var t = rest.split(" ")
      if (t.count == 1) {
        __val[name] = Num.fromString(t[0])
        __hasVal[name] = true
      } else {
        __left[name] = t[0]
        __op[name] = t[1]
        __right[name] = t[2]
        __hasVal[name] = false
      }
    }
    __hasVal["humn"] = false
    __op["root"] = "=="
    System.print(expect("root", 0))
  }
}

Monkey.run()
