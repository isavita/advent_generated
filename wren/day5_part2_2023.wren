
import "io" for File

var getNums = Fn.new { |str|
  var res = []
  var cur = ""
  for (c in str) {
    if ("0123456789".contains(c)) {
      cur = cur + c
    } else if (cur != "") {
      res.add(Num.fromString(cur))
      cur = ""
    }
  }
  if (cur != "") res.add(Num.fromString(cur))
  return res
}

var process = Fn.new { |ranges, rules|
  if (rules.count == 0) return ranges
  var q = []
  for (r in ranges) q.add([r[0], r[1]])
  var nextRanges = []
  while (q.count > 0) {
    var curr = q.removeAt(q.count - 1)
    var s = curr[0]
    var e = curr[1]
    var matched = false
    for (rule in rules) {
      var ms = rule[0]
      var me = rule[1]
      var off = rule[2]
      var os = s > ms ? s : ms
      var oe = e < me ? e : me
      if (os < oe) {
        nextRanges.add([os + off, oe + off])
        if (s < os) q.add([s, os])
        if (e > oe) q.add([oe, e])
        matched = true
        break
      }
    }
    if (!matched) nextRanges.add([s, e])
  }
  return nextRanges
}

var ranges = []
var rules = []
var lines = File.read("input.txt").split("\n")

for (line in lines) {
  if (line.startsWith("seeds:")) {
    var nums = getNums.call(line)
    var i = 0
    while (i < nums.count) {
      ranges.add([nums[i], nums[i] + nums[i+1]])
      i = i + 2
    }
  } else if (line.contains("map")) {
    if (rules.count > 0) {
      ranges = process.call(ranges, rules)
      rules = []
    }
  } else {
    var nums = getNums.call(line)
    if (nums.count == 3) {
      rules.add([nums[1], nums[1] + nums[2], nums[0] - nums[1]])
    }
  }
}
if (rules.count > 0) {
  ranges = process.call(ranges, rules)
}

var min = ranges[0][0]
for (r in ranges) {
  if (r[0] < min) min = r[0]
}
System.print(min)
