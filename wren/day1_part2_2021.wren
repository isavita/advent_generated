
import "io" for File

var content = File.read("input.txt")
var lines = content.split("\n")

var vals = []
for (line in lines) {
    if (line != "") {
        var n = Num.fromString(line)
        if (n) vals.add(n)
    }
}

if (vals.count < 3) {
    System.print(0)
} else {
    var prevSum = vals[0] + vals[1] + vals[2]
    var count = 0
    for (i in 3...vals.count) {
        var currSum = prevSum - vals[i-3] + vals[i]
        if (currSum > prevSum) count = count + 1
        prevSum = currSum
    }
    System.print(count)
}
