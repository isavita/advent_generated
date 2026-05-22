
import "io" for File

var content = File.read("input.txt").replace("\n", "").replace("\r", "")
var positions = content.split(",").map { |x| Num.fromString(x) }.toList
var max_pos = 0
for (p in positions) {
    if (p > max_pos) max_pos = p
}

var min_fuel = 1e18
for (target in 0..max_pos) {
    var fuel = 0
    for (p in positions) {
        fuel = fuel + (p - target).abs
    }
    if (fuel < min_fuel) min_fuel = fuel
}
System.print(min_fuel)
