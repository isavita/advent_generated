
import "io" for File

var parseLevels = Fn.new { |line|
  var levels = []
  for (field in line.split(" ")) {
    if (field != "") levels.add(Num.fromString(field))
  }
  return levels
}

var isSafeReport = Fn.new { |levels|
  if (levels.count < 2) return false
  var firstDiff = levels[1] - levels[0]
  if (firstDiff == 0) return false
  var isIncreasing = firstDiff > 0
  for (i in 0...levels.count-1) {
    var diff = levels[i+1] - levels[i]
    if (diff == 0) return false
    if ((isIncreasing && diff <= 0) || (!isIncreasing && diff >= 0)) return false
    if (diff.abs < 1 || diff.abs > 3) return false
  }
  return true
}

var safe = 0
var content = File.read("input.txt")
for (line in content.split("\n")) {
  if (line.trim() == "") continue
  if (isSafeReport.call(parseLevels.call(line))) safe = safe + 1
}
System.print(safe)
