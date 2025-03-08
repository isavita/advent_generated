
import Foundation

func main() {
    guard let originalMap = readMap(from: "input.txt") else {
        print("Error: Could not read the map.")
        return
    }

    let modifiedMap = modifyMap(originalMap)
    let robotPositions = findRobotPositions(in: modifiedMap)
    let (keys, doors, allKeys) = collectKeysAndDoors(in: modifiedMap)

    let keyPositions = createKeyPositions(from: keys, and: robotPositions)
    let keyGraph = buildKeyGraph(for: modifiedMap, keyPositions: keyPositions, keys: keys, robotPositions: robotPositions)

    let result = dijkstra(allKeys: allKeys, keyGraph: keyGraph, robotPositions: robotPositions)
    print(result)
}

func readMap(from file: String) -> [[Character]]? {
    guard let content = try? String(contentsOfFile: file) else { return nil }
    let lines = content.split(separator: "\n").map { String($0) }
    return lines.map { Array($0) }
}

func modifyMap(_ originalMap: [[Character]]) -> [[Character]] {
    var map = originalMap
    let rows = map.count
    let cols = map[0].count

    for y in 1..<rows - 1 {
        for x in 1..<cols - 1 {
            if map[y][x] == "@" &&
                map[y-1][x] == "." && map[y+1][x] == "." &&
                map[y][x-1] == "." && map[y][x+1] == "." {
                map[y-1][x-1...x+1] = ["@", "#", "@"]
                map[y][x-1...x+1] = ["#", "#", "#"]
                map[y+1][x-1...x+1] = ["@", "#", "@"]
                return map
            }
        }
    }
    print("Error: Could not find the '@' symbol surrounded by open spaces.")
    exit(1)
}

func findRobotPositions(in map: [[Character]]) -> [(Int, Int)] {
    var positions: [(Int, Int)] = []
    for (y, row) in map.enumerated() {
        for (x, cell) in row.enumerated() {
            if cell == "@" {
                positions.append((x, y))
            }
        }
    }
    return positions
}

func collectKeysAndDoors(in map: [[Character]]) -> ([Character: (Int, Int)], [Character: (Int, Int)], Set<Character>) {
    var keys: [Character: (Int, Int)] = [:]
    var doors: [Character: (Int, Int)] = [:]
    var allKeys: Set<Character> = []

    for (y, row) in map.enumerated() {
        for (x, cell) in row.enumerated() {
            if cell.isLowercase {
                keys[cell] = (x, y)
                allKeys.insert(cell)
            } else if cell.isUppercase {
                doors[cell] = (x, y)
            }
        }
    }
    return (keys, doors, allKeys)
}

func createKeyPositions(from keys: [Character: (Int, Int)], and robotPositions: [(Int, Int)]) -> [String: (Int, Int)] {
    var keyPositions: [String: (Int, Int)] = [:]
    for (key, pos) in keys {
        keyPositions[String(key)] = pos
    }
    for (i, pos) in robotPositions.enumerated() {
        keyPositions["@\(i)"] = pos
    }
    return keyPositions
}

func bfs(map: [[Character]], startPos: (Int, Int)) -> [Character: (Int, Set<Character>)] {
    var queue: [((Int, Int), Int, Set<Character>)] = [(startPos, 0, [])]
    var visited: Set<[Int]> = []
    var results: [Character: (Int, Set<Character>)] = [:]

    while !queue.isEmpty {
        let ((x, y), dist, requiredKeys) = queue.removeFirst()
        if visited.contains([x, y]) {
            continue
        }
        visited.insert([x,y])

        let cell = map[y][x]
        if cell.isLowercase && !requiredKeys.contains(cell) {
            results[cell] = (dist, requiredKeys)
        }

        for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let nx = x + dx
            let ny = y + dy

            if 0 <= ny && ny < map.count && 0 <= nx && nx < map[0].count {
                let ncell = map[ny][nx]
                if ncell != "#" {
                    var nextRequiredKeys = requiredKeys
                    if ncell.isUppercase {
                        nextRequiredKeys.insert(Character(ncell.lowercased()))
                    }
                    if cell.isLowercase{
                        nextRequiredKeys.insert(cell)
                    }
                    queue.append(((nx, ny), dist + 1, nextRequiredKeys))
                }
            }
        }
    }
    return results
}

func buildKeyGraph(for map: [[Character]], keyPositions: [String: (Int, Int)], keys: [Character: (Int, Int)], robotPositions: [(Int, Int)]) -> [String: [Character: (Int, Set<Character>)]] {
    var keyGraph: [String: [Character: (Int, Set<Character>)]] = [:]
    let allNodes = keys.keys.map { String($0) } + (0..<robotPositions.count).map { "@\($0)" }

    for key in allNodes {
        keyGraph[key] = keyBFS(map: map, startKey: key, keyPositions: keyPositions)
    }
    return keyGraph
}

func keyBFS(map: [[Character]], startKey: String, keyPositions: [String:(Int, Int)]) -> [Character: (Int, Set<Character>)]  {
    let startPos = keyPositions[startKey]!
    var queue: [((Int, Int), Int, Set<Character>)] = [(startPos, 0, [])]
    var visited: Set<[Int]> = []
    var results: [Character: (Int, Set<Character>)] = [:]

    while !queue.isEmpty {
        let ((x, y), dist, requiredKeys) = queue.removeFirst()

        if visited.contains([x,y]) {
            continue
        }
        visited.insert([x, y])

        let cell = map[y][x]
        if cell.isLowercase && String(cell) != startKey.lowercased() && !requiredKeys.contains(cell)  {
            results[cell] = (dist, requiredKeys)
        }

        for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let nx = x + dx
            let ny = y + dy

            if 0 <= ny && ny < map.count && 0 <= nx && nx < map[0].count {
                let ncell = map[ny][nx]
                if ncell != "#" {
                    var nextRequiredKeys = requiredKeys
                    if ncell.isUppercase{
                        nextRequiredKeys.insert(Character(ncell.lowercased()))
                    }

                    if cell.isLowercase {
                        nextRequiredKeys.insert(cell)
                    }
                    queue.append(((nx, ny), dist + 1, nextRequiredKeys))
                }
            }
        }
    }
    return results
}

func bitmask(for keysSet: Set<Character>) -> Int {
    var mask = 0
    for k in keysSet {
        mask |= 1 << (Int(k.asciiValue!) - Int(Character("a").asciiValue!))
    }
    return mask
}
struct State: Hashable {
    let positions: [String]
    let collectedKeys: Int
}
func dijkstra(allKeys: Set<Character>, keyGraph: [String: [Character: (Int, Set<Character>)]], robotPositions: [(Int, Int)]) -> Int? {
    let totalKeys = allKeys.count
    let initialPositions = (0..<robotPositions.count).map { "@\($0)" }
    var heap = [(0, initialPositions, Set<Character>())]
    var visited: [State: Int] = [:]

    while !heap.isEmpty {

        heap.sort { $0.0 < $1.0 }
        let (cost, positions, collectedKeys) = heap.removeFirst()
        let state = State(positions: positions, collectedKeys: bitmask(for: collectedKeys))

        if let visitedCost = visited[state], visitedCost <= cost {
            continue
        }

        visited[state] = cost

        if collectedKeys.count == totalKeys {
            return cost
        }

        for (i, pos) in positions.enumerated() {
            guard let availableKeys = keyGraph[pos] else { continue }

            for (key, (dist, requiredKeys)) in availableKeys {

                if !collectedKeys.contains(key) && requiredKeys.isSubset(of: collectedKeys) {

                    var newPositions = positions
                    newPositions[i] = String(key)
                    var newCollectedKeys = collectedKeys
                    newCollectedKeys.insert(key)

                    let newState = State(positions: newPositions, collectedKeys: bitmask(for: newCollectedKeys))

                    if let visitedCost = visited[newState], visitedCost <= cost + dist {
                        continue
                    }
                    heap.append((cost + dist, newPositions, newCollectedKeys))
                }
            }
        }
    }

    return nil
}

main()
