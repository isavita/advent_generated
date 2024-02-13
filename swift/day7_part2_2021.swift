
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
if let input = try? String(contentsOf: fileURL) {
    var positions = input.components(separatedBy: ",").compactMap { Int($0) }
    positions.sort()

    var minFuel = Int.max
    for i in positions[0]...positions[positions.count - 1] {
        var fuel = 0
        for pos in positions {
            fuel += calculateNewFuel(currentPosition: pos, newPosition: i)
        }
        if fuel < minFuel {
            minFuel = fuel
        }
    }
    print(minFuel)
}

func calculateNewFuel(currentPosition: Int, newPosition: Int) -> Int {
    let diff = abs(currentPosition - newPosition)
    return (diff * (diff + 1)) / 2
}

func abs(_ n: Int) -> Int {
    return n < 0 ? -n : n
}
