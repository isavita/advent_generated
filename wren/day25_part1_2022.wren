
import "io" for File

var fromSnafu = Fn.new { |s|
  var n = 0
  for (i in 0...s.count) {
    var ch = s[i]
    n = n * 5
    if (ch == "=") {
      n = n - 2
    } else if (ch == "-") {
      n = n - 1
    } else {
      n = n + Num.fromString(ch)
    }
  }
  return n
}

var toSnafu = Fn.new { |n|
  if (n == 0) return "0"
  var out = []
  while (n > 0) {
    var rem = n % 5
    if (rem == 3) {
      n = n + 5
      out.add("=")
    } else if (rem == 4) {
      n = n + 5
      out.add("-")
    } else {
      out.add(rem.toString)
    }
    n = (n / 5).floor
  }
  var rev = ""
  for (i in out.count-1..0) {
    rev = rev + out[i]
  }
  return rev
}

var sum = 0
var lines = File.read("input.txt").split("\n")
for (line in lines) {
  var s = line.trim()
  if (s.count > 0) {
    sum = sum + fromSnafu.call(s)
  }
}
System.print(toSnafu.call(sum))
