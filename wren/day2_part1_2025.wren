import "io" for File

var data = File.read("input.txt")
data = data.replace("\r\n", "").replace("\n", "").trim()

var parts = data.split(",")
var ranges = []
for (part in parts) {
  var pair = part.split("-")
  ranges.add([Num.fromString(pair[0]), Num.fromString(pair[1])])
}

var pow10 = Fn.new { |k|
  var r = 1
  for (i in 0...k) r = r * 10
  return r
}

var p10 = List.filled(11, 0)
for (i in 0..10) p10[i] = pow10.call(i)

var total = 0
var ceilDiv = Fn.new { |a, b| ((a + b - 1) / b).floor }

for (k in 1..10) {
  var mul = p10[k] + 1
  var minSeed = p10[k-1]
  var maxSeed = p10[k] - 1
  for (range in ranges) {
    var st = range[0]
    var en = range[1]
    var sMin = ceilDiv.call(st, mul)
    var sMax = (en / mul).floor
    var fromV = sMin > minSeed ? sMin : minSeed
    var toV = sMax < maxSeed ? sMax : maxSeed
    if (fromV <= toV) {
      var cnt = toV - fromV + 1
      total = total + (mul * (fromV + toV) * cnt / 2).floor
    }
  }
}

System.print(total)