
import "io" for File

var masses = []
var total = 0

var processLine = Fn.new { |line|
    var m = Num.fromString(line.trim())
    if (m != null) masses.add(m)
}

var calculateFuel = Fn.new { |mass|
    return (mass / 3).floor - 2
}

var getTotal = Fn.new {
    total = masses.map { |m| calculateFuel.call(m) }
                  .reduce(0) { |sum, fuel| sum + fuel }
}

var main = Fn.new {
    var contents = File.read("input.txt")
    contents.split("\n").each { |line| processLine.call(line) }
    getTotal.call()
    System.print(total)
}

main.call()
