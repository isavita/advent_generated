
import Foundation

func readInput() -> Int? {
    guard let data = try? String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines),
          let target = Int(data) else {
        return nil
    }
    return target
}

func solve(target: Int) -> Int {
    var grid = [String: Int]()
    grid["0,0"] = 1

    var x = 0, y = 0
    var dx = 0, dy = -1

    while true {
        if x == y || (x < 0 && x == -y) || (x > 0 && x == 1 - y) {
            (dx, dy) = (-dy, dx)
        }

        x += dx
        y += dy

        let key = "\(x),\(y)"
        let value = (-1...1).flatMap { dx in
            (-1...1).map { dy in
                grid["\(x + dx),\(y + dy)"] ?? 0
            }
        }.reduce(0, +)

        grid[key] = value

        if value > target {
            return value
        }
    }
}

if let target = readInput() {
    print(solve(target: target))
}
