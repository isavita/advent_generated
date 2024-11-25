
import "io" for File

var input = File.read("input.txt").trim()
var startingNumbers = input.split(",")

var spoken = {}
var lastSpoken = 0

for (i in 0...startingNumbers.count) {
    var num = Num.fromString(startingNumbers[i])
    if (i == startingNumbers.count - 1) {
        lastSpoken = num
    } else {
        spoken[num] = i + 1
    }
}

for (turn in startingNumbers.count + 1..30000000) {
    var nextNumber = 0
    var lastTurn = spoken[lastSpoken]
    
    if (lastTurn != null) {
        nextNumber = turn - 1 - lastTurn
    }
    
    spoken[lastSpoken] = turn - 1
    lastSpoken = nextNumber
}

System.print(lastSpoken)
