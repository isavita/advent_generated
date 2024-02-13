
import Foundation

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)

var distances: [String: [String: Int]] = [:]

input.components(separatedBy: .newlines).forEach { line in
    let parts = line.components(separatedBy: " ")
    let city1 = parts[0]
    let city2 = parts[2]
    let distance = Int(parts[4])!

    if distances[city1] == nil {
        distances[city1] = [:]
    }
    distances[city1]![city2] = distance

    if distances[city2] == nil {
        distances[city2] = [:]
    }
    distances[city2]![city1] = distance
}

func calculateDistance(route: [String]) -> Int {
    var distance = 0
    for i in 0..<route.count - 1 {
        distance += distances[route[i]]![route[i + 1]]!
    }
    return distance
}

var cities = Array(distances.keys)
var minDistance = Int.max
var maxDistance = 0

func permute(_ a: [String], _ n: Int) {
    var a = a // Make a mutable copy of 'a'
    if n == 1 {
        let distance = calculateDistance(route: a)
        minDistance = min(minDistance, distance)
        maxDistance = max(maxDistance, distance)
    } else {
        for i in 0..<n {
            a.swapAt(i, n - 1)
            permute(a, n - 1)
            a.swapAt(i, n - 1)
        }
    }
}

permute(cities, cities.count)

print("Shortest distance: \(minDistance)")
print("Longest distance: \(maxDistance)")
