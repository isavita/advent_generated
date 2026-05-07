
import "io" for File

var Rules = {}
var Memo = {}

var getVal // define later for recursion

getVal = Fn.new { |x|
  var n = Num.fromString(x)
  if (n != null) return n
  if (Memo.containsKey(x)) return Memo[x]

  var parts = Rules[x].split(" ")
  var res = 0

  if (parts.count == 1) {
    res = getVal.call(parts[0])
  } else if (parts.count == 2) {
    res = ~getVal.call(parts[1])
  } else {
    var a = parts[0]
    var op = parts[1]
    var b = parts[2]
    if (op == "AND") res = getVal.call(a) & getVal.call(b)
    if (op == "OR") res = getVal.call(a) | getVal.call(b)
    if (op == "LSHIFT") res = getVal.call(a) << getVal.call(b)
    if (op == "RSHIFT") res = getVal.call(a) >> getVal.call(b)
  }

  Memo[x] = res & 0xFFFF
  return Memo[x]
}

var lines = File.read("input.txt").trim().split("\n")
for (line in lines) {
  var p = line.split(" -> ")
  Rules[p[1].trim()] = p[0].trim()
}

System.print(getVal.call("a"))
