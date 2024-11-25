
import "io" for File

var input = File.read("input.txt").trim()
var startingNumbers = input.split(",")

var lastSpoken = {}
var lastNumber = 0
var nextNumber = 0

for (turn in 1..2020) {
    if (turn - 1 < startingNumbers.count) {
        lastNumber = Num.fromString(startingNumbers[turn - 1])
        lastSpoken[lastNumber] = turn
        continue
    }
    
    var lastTurn = lastSpoken[lastNumber]
    if (lastTurn != null && lastTurn != turn - 1) {
        nextNumber = turn - 1 - lastTurn
    } else {
        nextNumber = 0
    }
    
    lastSpoken[lastNumber] = turn - 1
    lastNumber = nextNumber
}

System.print(lastNumber)
