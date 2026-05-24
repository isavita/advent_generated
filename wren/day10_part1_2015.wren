
import "io" for File

var input = File.read("input.txt").trim()
var bytes = input.bytes.toList

for (i in 0...40) {
    var nextBytes = []
    var len = bytes.count
    if (len > 0) {
        var prev = bytes[0]
        var count = 1
        for (j in 1...len) {
            var curr = bytes[j]
            if (curr == prev) {
                count = count + 1
            } else {
                if (count < 10) {
                    nextBytes.add(count + 48)
                } else {
                    for (b in count.toString.bytes) nextBytes.add(b)
                }
                nextBytes.add(prev)
                prev = curr
                count = 1
            }
        }
        if (count < 10) {
            nextBytes.add(count + 48)
        } else {
            for (b in count.toString.bytes) nextBytes.add(b)
        }
        nextBytes.add(prev)
    }
    bytes = nextBytes
}

System.print(bytes.count)
