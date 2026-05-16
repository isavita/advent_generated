
import "io" for File

var data = File.read("input.txt")
var lines = data.split("\n")

var total = 0
var counts = List.filled(26, 0)   // one slot per letter a‑z
var groupSize = 0

for (line in lines) {
    if (line == "") {
        if (groupSize > 0) {
            // count letters that appeared in every line of the group
            for (i in 0...26) {
                if (counts[i] == groupSize) total = total + 1
            }
            // reset for the next group
            for (i in 0...26) counts[i] = 0
            groupSize = 0
        }
    } else {
        groupSize = groupSize + 1
        for (ch in line) {
            var idx = ch.codePoints[0] - 97   // 'a' → 0, 'b' → 1, …
            counts[idx] = counts[idx] + 1
        }
    }
}

// last group (no trailing blank line)
if (groupSize > 0) {
    for (i in 0...26) {
        if (counts[i] == groupSize) total = total + 1
    }
}

System.print(total)
