import"io" for File

var readInput = Fn.new {
    var content = File.read("input.txt")
    var lines = content.split("\n")
    var expenses = []
    for (line in lines) {
        var trimmed = line.trim()
        if (trimmed == "") continue
        var n = Num.fromString(trimmed)
        if (n) expenses.add(n)
    }
    return expenses
}

var expenses = readInput.call()

var solvePart1 = Fn.new { |expenses|
    var n = expenses.count
    for (i in 0...n) {
        for (j in i+1...n) {
            if (expenses[i] + expenses[j] == 2020) {
                return expenses[i] * expenses[j]
            }
        }
    }
    return 0
}

var solvePart2 = Fn.new { |expenses|
    var n = expenses.count
    for (i in 0...n) {
        for (j in i+1...n) {
            for (k in j+1...n) {
                if (expenses[i] + expenses[j] + expenses[k] == 2020) {
                    return expenses[i] * expenses[j] * expenses[k]
                }
            }
        }
    }
    return 0
}

System.print("Part 1: %(solvePart1.call(expenses))")
System.print("Part 2: %(solvePart2.call(expenses))")