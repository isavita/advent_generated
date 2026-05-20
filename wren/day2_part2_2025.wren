
import "io" for File

var pow = Fn.new { |b, e|
  var r = 1
  for (i in 0...e) r = r * b
  return r
}

var content = File.read("input.txt").replace("\r", "").replace("\n", "")
var found = {}
for (part in content.split(",")) {
  if (part == "") continue
  var b = part.split("-")
  var rstart = Num.fromString(b[0])
  var rend = Num.fromString(b[1])
  var sLen = b[0].count
  var eLen = b[1].count
  for (totalLen in sLen..eLen) {
    var limit = (totalLen / 2).floor
    if (limit >= 1) {
      for (k in 1..limit) {
        if (totalLen % k != 0) continue
        var reps = (totalLen / k).floor
        var M = 0
        for (j in 0...reps) M = M + pow.call(10, j * k)
        var minSeed = pow.call(10, k - 1)
        var maxSeed = pow.call(10, k) - 1
        var targetMin = ((rstart + M - 1) / M).floor
        var targetMax = (rend / M).floor
        var start = targetMin > minSeed ? targetMin : minSeed
        var end = targetMax < maxSeed ? targetMax : maxSeed
        if (start <= end) {
          for (seed in start..end) {
            found[seed * M] = true
          }
        }
      }
    }
  }
}

var sum = 0
for (k in found.keys) {
  sum = sum + k
}
System.print(sum)
