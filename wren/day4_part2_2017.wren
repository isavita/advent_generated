
import "io" for File

var sortWord = Fn.new { |s|
    var counts = List.filled(26, 0)
    for (b in s.bytes) {
        counts[b - 97] = counts[b - 97] + 1
    }
    var parts = []
    for (i in 0..25) {
        if (counts[i] > 0) parts.add(String.fromByte(97 + i) * counts[i])
    }
    return parts.join()
}

var input = File.read("input.txt")
var lines = input.trimEnd().split("\n")
var validCount = 0

for (line in lines) {
    if (line == "") continue
    var seen = {}
    var valid = true
    for (word in line.split(" ")) {
        if (word == "") continue
        var sw = sortWord.call(word)
        if (seen.containsKey(sw)) {
            valid = false
            break
        }
        seen[sw] = true
    }
    if (valid) validCount = validCount + 1
}

System.print(validCount)
