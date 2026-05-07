
import "io" for File

var content = File.read("input.txt").trim()
var lines = content.split("\n")

var bits = lines[0].count
var gammaRate = ""
var epsilonRate = ""

for (i in 0...bits) {
    var ones = 0
    for (line in lines) {
        if (line.bytes[i] == 49) ones = ones + 1  // 49 is '1'
    }
    var zeros = lines.count - ones
    if (ones >= zeros) {
        gammaRate = gammaRate + "1"
        epsilonRate = epsilonRate + "0"
    } else {
        gammaRate = gammaRate + "0"
        epsilonRate = epsilonRate + "1"
    }
}

var binaryToInt = Fn.new {|s|
    var n = 0
    for (ch in s) {
        n = n * 2
        if (ch == "1") n = n + 1
    }
    return n
}

var gamma = binaryToInt.call(gammaRate)
var epsilon = binaryToInt.call(epsilonRate)

System.print("Power consumption: %(gamma * epsilon)")
