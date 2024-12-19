
import Foundation

struct Move {
    let label: String
    let x: Int
    let y: Int
}

func solve() -> Int {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        fatalError("Failed to read input.txt")
    }
    
    let lines = input.split(separator: "\n").filter { !$0.isEmpty }
    var graph = lines.map { Array($0) }
    
    let H = graph.count
    let W = graph[0].count
    
    let moves = [
        Move(label: "left", x: -1, y: 0),
        Move(label: "up", x: 0, y: -1),
        Move(label: "right", x: 1, y: 0),
        Move(label: "down", x: 0, y: 1)
    ]
    
    var sum = 0
    
    for y in 0..<H {
        for x in 0..<W {
            guard graph[y][x] != "." else { continue }
            
            var area = 0
            let target = graph[y][x]
            var visited = Set<String>()
            var side = [String: Set<String>]()
            
            func search(cx: Int, cy: Int, label: String) {
                guard cx >= 0 && cx < W && cy >= 0 && cy < H else {
                    saveOuter(label: label, side: &side, x: cx, y: cy)
                    return
                }
                
                guard graph[cy][cx] == target else {
                    if !label.isEmpty && !visited.contains("\(cx),\(cy)") {
                        saveOuter(label: label, side: &side, x: cx, y: cy)
                    }
                    return
                }
                
                visited.insert("\(cx),\(cy)")
                area += 1
                graph[cy][cx] = "."
                
                for move in moves {
                    search(cx: cx + move.x, cy: cy + move.y, label: move.label)
                }
            }
            
            search(cx: x, cy: y, label: "")
            let outer = countOuter(side: side)
            sum += area * outer
        }
    }
    return sum
}

func saveOuter(label: String, side: inout [String: Set<String>], x: Int, y: Int) {
    let key: String
    if label == "up" || label == "down" {
        key = "\(y):\(x)"
    } else {
        key = "\(x):\(y)"
    }
    side[label, default: []].insert(key)
}

func countOuter(side: [String: Set<String>]) -> Int {
    var outer = 0
    for (_, keys) in side {
        let sortedKeys = keys.sorted { (key1, key2) -> Bool in
            let parts1 = key1.split(separator: ":").compactMap { Int($0) }
            let parts2 = key2.split(separator: ":").compactMap { Int($0) }
            if parts1[0] == parts2[0] {
                return parts1[1] < parts2[1]
            }
            return parts1[0] < parts2[0]
        }
        
        var temp = [String]()
        for current in sortedKeys {
            let parts = current.split(separator: ":").compactMap { Int($0) }
            if !check(ary: temp, i: parts[0], j: parts[1]) {
                outer += 1
            }
            temp.append(current)
        }
    }
    return outer
}

func check(ary: [String], i: Int, j: Int) -> Bool {
    let search = ["\(i):\(j-1)", "\(i):\(j+1)"]
    for s in search {
        if ary.contains(s) {
            return true
        }
    }
    return false
}

print(solve())
