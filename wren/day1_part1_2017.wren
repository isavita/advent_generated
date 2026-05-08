import "io" for File

var input = File.read("input.txt").trim()
var sum = 0
var len = input.count
for (i in 0...len) {
    var cur = input[i]
    var next = input[(i + 1) % len]
    if (cur == next) {
        sum = sum + Num.fromString(cur)
    }
}
System.print(sum)