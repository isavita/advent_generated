
import "io" for File

var itemPriority = Fn.new { |byte|
    if (byte >= 97) return byte - 96
    return byte - 38
}

var content = File.read("input.txt")
var lines = content.split("\n").where { |line| line != "" }.toList
var sum = 0

for (line in lines) {
    var bytes = line.bytes
    var n = bytes.count
    var half = (n/2).floor
    var firstSet = {}
    for (i in 0...half) {
        firstSet[bytes[i]] = true
    }
    for (i in half...n) {
        var b = bytes[i]
        if (firstSet.containsKey(b)) {
            sum = sum + itemPriority.call(b)
            break
        }
    }
}

System.print(sum)
