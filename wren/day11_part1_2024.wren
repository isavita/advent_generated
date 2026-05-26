
import "io" for File

var trim = Fn.new { |s|
    var i = 0
    while (i < s.count && (s[i] == " " || s[i] == "\n" || s[i] == "\r" || s[i] == "\t")) {
        i = i + 1
    }
    var j = s.count - 1
    while (j >= i && (s[j] == " " || s[j] == "\n" || s[j] == "\r" || s[j] == "\t")) {
        j = j - 1
    }
    return i > j ? "" : s[i..j]
}

var step = Fn.new { |counts|
    var nextCounts = {}
    for (num in counts.keys) {
        var count = counts[num]
        if (num == 0) {
            nextCounts[1] = (nextCounts[1] || 0) + count
        } else {
            var s = num.toString
            if (s.count % 2 == 0) {
                var mid = (s.count / 2).floor
                var left = Num.fromString(s[0...mid])
                var right = Num.fromString(s[mid...s.count])
                nextCounts[left] = (nextCounts[left] || 0) + count
                nextCounts[right] = (nextCounts[right] || 0) + count
            } else {
                var nextNum = num * 2024
                nextCounts[nextNum] = (nextCounts[nextNum] || 0) + count
            }
        }
    }
    return nextCounts
}

var content = File.read("input.txt")
var counts = {}
for (x in trim.call(content).split(" ")) {
    if (x != "") {
        var num = Num.fromString(x)
        counts[num] = (counts[num] || 0) + 1
    }
}

for (i in 1..25) {
    counts = step.call(counts)
}

var total = 0
for (num in counts.keys) {
    total = total + counts[num]
}
System.print(total)
