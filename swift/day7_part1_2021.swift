
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let fileContents = try String(contentsOf: fileURL)
let lines = fileContents.components(separatedBy: .newlines)

var positions: [Int] = []
for line in lines {
    let numbers = line.components(separatedBy: ",")
    for num_str in numbers {
        if let num = Int(num_str) {
            positions.append(num)
        }
    }
}

positions.sort()

var min_fuel = Int.max
for i in positions[0]...positions[positions.count - 1] {
    var fuel = 0
    for pos in positions {
        fuel += calculateFuel(currentPosition: pos, newPosition: i)
    }
    if fuel < min_fuel {
        min_fuel = fuel
    }
}
print(min_fuel)

func calculateFuel(currentPosition: Int, newPosition: Int) -> Int {
    return abs(currentPosition - newPosition)
}

func abs(_ n: Int) -> Int {
    return n < 0 ? -n : n
}
