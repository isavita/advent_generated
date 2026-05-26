import "io" for File

var content = File.read("input.txt")
var res = 0

for (line in content.split("\n")) {
    var curr = []
    for (part in line.split(" ")) {
        var trimmed = part.trim()
        if (trimmed != "") {
            curr.add(Num.fromString(trimmed))
        }
    }
    if (curr.count == 0) continue

    var lastsSum = 0
    while (true) {
        lastsSum = lastsSum + curr[-1]
        var allZero = true
        var nextSeq = []
        var len = curr.count - 1
        for (i in 0...len) {
            var diff = curr[i+1] - curr[i]
            nextSeq.add(diff)
            if (diff != 0) {
                allZero = false
            }
        }
        if (allZero) {
            break
        }
        curr = nextSeq
    }
    res = res + lastsSum
}

System.print(res)