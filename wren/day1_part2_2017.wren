
import "io" for File

var content = File.read("input.txt").trim()
var sum1 = 0
var sum2 = 0

for (line in content.split("\n")) {
    line = line.trim()
    if (line.count > 0) {
        var digits = line.bytes.map { |b| b - 48 }.toList
        var len = digits.count
        var half = (len / 2).floor
        for (i in 0...len) {
            if (digits[i] == digits[(i + 1) % len]) sum1 = sum1 + digits[i]
            if (digits[i] == digits[(i + half) % len]) sum2 = sum2 + digits[i]
        }
    }
}

System.print("Part 1: %(sum1)")
System.print("Part 2: %(sum2)")
