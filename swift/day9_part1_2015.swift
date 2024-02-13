
import Foundation

func readAndParseInput(filename: String) -> [String: [String: Int]] {
    let fileURL = URL(fileURLWithPath: filename)
    guard let input = try? String(contentsOf: fileURL) else {
        return [:]
    }
    
    var distances: [String: [String: Int]] = [:]
    let lines = input.components(separatedBy: .newlines)
    for line in lines {
        let parts = line.components(separatedBy: " ")
        if parts.count != 5 {
            continue
        }
        
        let from = parts[0]
        let to = parts[2]
        let dist = Int(parts[4]) ?? 0
        
        if distances[from] == nil {
            distances[from] = [:]
        }
        distances[from]![to] = dist
        
        if distances[to] == nil {
            distances[to] = [:]
        }
        distances[to]![from] = dist
    }
    
    return distances
}

func getUniqueLocations(distances: [String: [String: Int]]) -> [String] {
    var locationSet: Set<String> = []
    for (from, _) in distances {
        locationSet.insert(from)
        for (to, _) in distances[from]! {
            locationSet.insert(to)
        }
    }
    
    return Array(locationSet)
}

func findShortestRoute(locations: [String], distances: [String: [String: Int]]) -> Int {
    var minDistance = -1
    var mutableLocations = locations // Make a mutable copy of locations
    permute(&mutableLocations, 0, &minDistance, distances) // Pass the mutable copy
    return minDistance
}

func permute(_ arr: inout [String], _ i: Int, _ minDistance: inout Int, _ distances: [String: [String: Int]]) {
    if i > arr.count {
        return
    }
    if i == arr.count {
        let dist = calculateRouteDistance(arr, distances)
        if minDistance == -1 || dist < minDistance {
            minDistance = dist
        }
        return
    }
    for j in i..<arr.count {
        arr.swapAt(i, j)
        permute(&arr, i+1, &minDistance, distances)
        arr.swapAt(i, j)
    }
}

func calculateRouteDistance(_ route: [String], _ distances: [String: [String: Int]]) -> Int {
    var sum = 0
    for i in 0..<route.count-1 {
        sum += distances[route[i]]![route[i+1]]!
    }
    return sum
}

let distances = readAndParseInput(filename: "input.txt")
let locations = getUniqueLocations(distances: distances)
let minDistance = findShortestRoute(locations: locations, distances: distances)
print(minDistance)
