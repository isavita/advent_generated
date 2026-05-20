
import "io" for File

var trim = Fn.new { |s|
    var start = 0
    while (start < s.count && (s[start] == " " || s[start] == "\t")) {
        start = start + 1
    }
    if (start == s.count) return ""
    var end = s.count - 1
    while (end >= start && (s[end] == " " || s[end] == "\t")) {
        end = end - 1
    }
    return s[start..end]
}

var isDigit = Fn.new { |s|
    if (s.isEmpty) return false
    for (b in s.bytes) {
        if (b < 48 || b > 57) return false
    }
    return true
}

var lines = File.read("input.txt").replace("\r", "").split("\n")
if (lines.count > 0 && lines[-1] == "") lines.removeAt(-1)

var maxLen = 0
for (line in lines) {
    if (line.count > maxLen) maxLen = line.count
}

var sep = List.filled(maxLen, true)
for (c in 0...maxLen) {
    for (line in lines) {
        if (c < line.count) {
            var ch = line[c]
            if (ch != " " && ch != "\t") {
                sep[c] = false
                break
            }
        }
    }
}

var block = Fn.new { |s, e|
    var op = ""
    var cnt = 0
    var sum = 0
    var prod = 1
    for (line in lines) {
        if (s < line.count) {
            var end = e.min(line.count - 1)
            var seg = trim.call(line[s..end])
            if (seg != "") {
                if (seg == "+" || seg == "*") {
                    op = seg
                } else if (isDigit.call(seg)) {
                    cnt = cnt + 1
                    var val = Num.fromString(seg)
                    sum = sum + val
                    prod = prod * val
                }
            }
        }
    }
    if (cnt == 0) return 0
    if (op == "+") return sum
    if (op == "*") return prod
    if (cnt == 1) return sum
    return 0
}

var total = 0
var inb = false
var start = 0
for (c in 0...maxLen) {
    if (!sep[c]) {
        if (!inb) {
            inb = true
            start = c
        }
    } else if (inb) {
        total = total + block.call(start, c - 1)
        inb = false
    }
}
if (inb) total = total + block.call(start, maxLen - 1)

System.print("Grand total: %(total)")
