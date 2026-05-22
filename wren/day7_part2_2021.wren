
import "io" for File

var content = File.read("input.txt").trim()
var positions = content.split(",").map { |x| Num.fromString(x.trim()) }.toList

var sum = 0
for (p in positions) sum = sum + p
var mean = sum / positions.count

var cost = Fn.new { |t|
    var total = 0
    for (p in positions) {
        var d = (p - t).abs
        total = total + (d * (d + 1) / 2).floor
    }
    return total
}

var min_fuel = cost.call(mean.floor)
var c2 = cost.call(mean.ceil)
if (c2 < min_fuel) min_fuel = c2

System.print(min_fuel)
