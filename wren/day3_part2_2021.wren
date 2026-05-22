
import "io" for File

var toDec = Fn.new { |s|
  var v = 0
  for (c in s) v = v * 2 + (c == "1" ? 1 : 0)
  return v
}

var lines = File.read("input.txt").trim().replace("\r", "").split("\n").where { |x| x != "" }.toList

var f = Fn.new { |m|
  var b = lines.toList
  var L = b[0].count
  for (j in 0...L) {
    if (b.count <= 1) break
    var c1 = 0
    for (s in b) if (s[j] == "1") c1 = c1 + 1
    var c0 = b.count - c1
    var t = m == "o" ? (c1 >= c0 ? "1" : "0") : (c0 <= c1 ? "0" : "1")
    b = b.where { |s| s[j] == t }.toList
  }
  return b[0]
}

System.print(toDec.call(f.call("o")) * toDec.call(f.call("c")))
