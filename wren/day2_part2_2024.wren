
import "io" for File

var abs = Fn.new { |x| x < 0 ? -x : x }

var isSafe = Fn.new { |arr|
    var n = arr.count
    if (n < 2) return false
    var firstDiff = arr[1] - arr[0]
    if (firstDiff == 0) return false
    var inc = firstDiff > 0
    for (i in 0...(n - 1)) {
        var diff = arr[i+1] - arr[i]
        if (diff == 0) return false
        if (inc && diff <= 0) return false
        if (!inc && diff >= 0) return false
        var ad = abs.call(diff)
        if (ad < 1 || ad > 3) return false
    }
    return true
}

var isSafeWithOneRemoval = Fn.new { |arr|
    var n = arr.count
    if (n <= 2) return false
    for (i in 0...n) {
        var tmp = []
        for (j in 0...n) {
            if (j != i) tmp.add(arr[j])
        }
        if (isSafe.call(tmp)) return true
    }
    return false
}

var content = File.read("input.txt")
var safeCount = 0
for (line in content.split("\n")) {
    var levels = []
    var cur = ""
    for (c in line) {
        if (c == " " || c == "\t" || c == "\r" || c == "\n") {
            if (cur != "") {
                levels.add(Num.fromString(cur))
                cur = ""
            }
        } else {
            cur = cur + c
        }
    }
    if (cur != "") levels.add(Num.fromString(cur))
    
    if (levels.count > 0) {
        if (isSafe.call(levels) || isSafeWithOneRemoval.call(levels)) {
            safeCount = safeCount + 1
        }
    }
}
System.print(safeCount)
