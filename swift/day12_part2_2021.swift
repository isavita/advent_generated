
import Foundation

func solve() {
    guard let input = try? String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines) else {
        fatalError("Invalid input")
    }
    let parsed = input.components(separatedBy: "\n").map { $0.components(separatedBy: "-") }
    var graph: [String: Set<String>] = [:]
    for pair in parsed {
        graph[pair[0], default: []].insert(pair[1])
        graph[pair[1], default: []].insert(pair[0])
    }

    func isSmallCave(_ cave: String) -> Bool {
        return cave.lowercased() == cave
    }

    func walk(current: String, visited: Set<String>, smallCaveVisitedTwice: Bool) -> Int {
        if current == "end" {
            return 1
        }
        var pathsToEnd = 0
        for neighbor in graph[current]! {
            if neighbor == "start" {
                continue
            }
            let isSmall = isSmallCave(neighbor)
            if isSmall && visited.contains(neighbor) {
                if smallCaveVisitedTwice {
                    continue
                }
                pathsToEnd += walk(current: neighbor, visited: visited, smallCaveVisitedTwice: true)
            } else {
                var newVisited = visited
                if isSmall {
                    newVisited.insert(neighbor)
                }
                pathsToEnd += walk(current: neighbor, visited: newVisited, smallCaveVisitedTwice: smallCaveVisitedTwice)
            }
        }
        return pathsToEnd
    }

    print(walk(current: "start", visited: ["start"], smallCaveVisitedTwice: false))
}

solve()
