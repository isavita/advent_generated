
import "io" for File

var hasDoubleAndIncreasing = Fn.new { |n|
    var hasDouble = false
    var temp = n
    while (temp > 0) {
        var digit = temp % 10
        temp = (temp / 10).floor
        if (temp > 0) {
            var prev = temp % 10
            if (prev > digit) return false
            if (prev == digit) hasDouble = true
        }
    }
    return hasDouble
}

var content = File.read("input.txt").trim()
var parts = content.split("-")
var start = Num.fromString(parts[0])
var end = Num.fromString(parts[1])

var count = 0
for (i in start..end) {
    if (hasDoubleAndIncreasing.call(i)) {
        count = count + 1
    }
}

System.print(count)
