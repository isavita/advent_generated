
import "io" for File

var x = [1]
var lines = File.read("input.txt").split("\n")

for (line in lines) {
    if (line == "noop") {
        x.add(x[-1])
    } else {
        var n = Num.fromString(line.split(" ")[1])
        x.add(x[-1])
        x.add(x[-1] + n)
    }
}

var sum = 0
for (i in 0...x.count) {
    if ((i - 19) % 40 == 0) {
        sum = sum + (i + 1) * x[i]
    }
}

System.print(sum)
