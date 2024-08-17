import "io" for File

var main = Fn.new {
    var input = File.read("input.txt").trim()
    var steps = Num.fromString(input)
    var currentPos = 0
    var valueAfterZero = 0

    for (i in 1..50000000) {
        currentPos = (currentPos + steps) % i
        if (currentPos == 0) {
            valueAfterZero = i
        }
        currentPos = currentPos + 1
    }

    System.print(valueAfterZero)
}

main.call()