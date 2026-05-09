
import "io" for File

var parseNumbers = Fn.new { |s|
    var nums = []
    for (token in s.split(" ")) {
        var trimmed = token.trim()
        if (trimmed != "") {
            nums.add(Num.fromString(trimmed))
        }
    }
    return nums
}

var calculateMatches = Fn.new { |line|
    var parts = line.split(": ")
    var numberParts = parts[1].split(" | ")
    var winningNumbers = parseNumbers.call(numberParts[0])
    var givenNumbers = parseNumbers.call(numberParts[1])
    var winSet = {}
    for (num in winningNumbers) {
        winSet[num] = true
    }
    var count = 0
    for (num in givenNumbers) {
        if (winSet[num] == true) count = count + 1
    }
    return count
}

var main = Fn.new {
    var inputData = File.read("input.txt").trim()
    var lines = []
    for (line in inputData.split("\n")) {
        var trimmed = line.trim()
        if (trimmed != "") lines.add(trimmed)
    }
    if (lines.isEmpty) {
        System.print(0)
        return
    }
    var matches = []
    for (line in lines) {
        matches.add(calculateMatches.call(line))
    }
    var numCards = lines.count
    var counts = []
    for (i in 0...numCards) counts.add(1)
    for (i in 0...numCards) {
        var m = matches[i]
        if (m > 0) {
            var current = counts[i]
            for (j in 1..m) {
                var idx = i + j
                if (idx < numCards) {
                    counts[idx] = counts[idx] + current
                }
            }
        }
    }
    var total = 0
    for (c in counts) total = total + c
    System.print(total)
}

main.call()
