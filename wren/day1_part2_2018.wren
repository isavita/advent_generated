import "io" for File

var input = File.read("input.txt").trim().split("\n")
var frequencies = {}
var currentFrequency = 0
frequencies[currentFrequency] = true

while (true) {
    for (change in input) {
        var frequencyDelta = Num.fromString(change)
        currentFrequency = currentFrequency + frequencyDelta
        if (frequencies[currentFrequency]) {
            System.print(currentFrequency)
            return
        }
        frequencies[currentFrequency] = true
    }
}