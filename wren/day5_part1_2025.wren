
import "io" for File

var content = File.read("input.txt")
var lines = content.split("\n")

var ranges = []
var i = 0
while (i < lines.count) {
  var line = lines[i].trim()
  if (line == "") break
  var parts = line.split("-")
  if (parts.count == 2) {
    var low = Num.fromString(parts[0])
    var high = Num.fromString(parts[1])
    ranges.add([low, high])
  }
  i = i + 1
}
i = i + 1

var ids = []
while (i < lines.count) {
  var line = lines[i].trim()
  if (line != "") {
    line = line.replace("\t", " ")
    var tokens = line.split(" ")
    for (token in tokens) {
      if (token != "") ids.add(Num.fromString(token))
    }
  }
  i = i + 1
}

var sortRanges = Fn.new {|ranges|
  for (i in 0...ranges.count) {
    for (j in i+1...ranges.count) {
      if (ranges[j][0] < ranges[i][0]) {
        var tmp = ranges[i]
        ranges[i] = ranges[j]
        ranges[j] = tmp
      }
    }
  }
}

var merge = Fn.new {|ranges|
  if (ranges.count == 0) return []
  sortRanges.call(ranges)
  var merged = []
  var cur = ranges[0]
  for (i in 1...ranges.count) {
    var nxt = ranges[i]
    if (nxt[0] <= cur[1] + 1) {
      if (nxt[1] > cur[1]) cur[1] = nxt[1]
    } else {
      merged.add(cur)
      cur = nxt
    }
  }
  merged.add(cur)
  return merged
}

var mergedRanges = merge.call(ranges)

var contains = Fn.new {|id, mergedRanges|
  var lo = 0
  var hi = mergedRanges.count - 1
  while (lo <= hi) {
    var mid = ((lo + hi) / 2).floor
    var r = mergedRanges[mid]
    if (id < r[0]) {
      hi = mid - 1
    } else if (id > r[1]) {
      lo = mid + 1
    } else {
      return true
    }
  }
  return false
}

var cnt = 0
for (id in ids) {
  if (contains.call(id, mergedRanges)) cnt = cnt + 1
}
System.print(cnt)
