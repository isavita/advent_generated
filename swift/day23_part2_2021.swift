
import Foundation

protocol HeapNode {
    func value() -> Int
}

struct MinHeap {
    private var nodes: [HeapNode] = []
    
    mutating func add(_ newNode: HeapNode) {
        nodes.append(newNode)
        heapifyFromEnd()
    }
    
    mutating func remove() -> HeapNode? {
        guard !nodes.isEmpty else { return nil }
        let rootNode = nodes[0]
        nodes[0] = nodes.last!
        nodes.removeLast()
        heapifyFromStart()
        return rootNode
    }
    
    func length() -> Int {
        return nodes.count
    }
    
    private mutating func heapifyFromEnd() {
        var currentIndex = nodes.count - 1
        while currentIndex > 0 {
            let parentIndex = (currentIndex - 1) / 2
            if nodes[currentIndex].value() < nodes[parentIndex].value() {
                nodes.swapAt(parentIndex, currentIndex)
                currentIndex = parentIndex
            } else {
                break
            }
        }
    }
    
    private mutating func heapifyFromStart() {
        var currentIndex = 0
        while true {
            var smallerChildIndex = currentIndex
            for i in 1...2 {
                let childIndex = currentIndex * 2 + i
                if childIndex < nodes.count && nodes[childIndex].value() < nodes[smallerChildIndex].value() {
                    smallerChildIndex = childIndex
                }
            }
            if smallerChildIndex == currentIndex { return }
            nodes.swapAt(smallerChildIndex, currentIndex)
            currentIndex = smallerChildIndex
        }
    }
}

struct State: HeapNode, CustomStringConvertible {
    var grid: [[String]]
    var energyUsed: Int
    var path: String
    
    func value() -> Int {
        return energyUsed
    }
    
    var description: String {
        var sb = ""
        for row in grid {
            for char in row {
                sb.append(char)
            }
            sb.append("\n")
        }
        sb.append("nrg: \(energyUsed), path: \(path)\n")
        return sb
    }
    
    func copy() -> State {
        return State(grid: grid, energyUsed: energyUsed, path: path)
    }
    
    func allDone(roomCoordToWantChar: [[Int]: String]) -> Bool {
        for (coord, want) in roomCoordToWantChar {
            if grid[coord[0]][coord[1]] != want {
                return false
            }
        }
        return true
    }
    
    func getUnsettledCoords(roomCoordToWantChar: [[Int]: String]) -> [[Int]] {
        var unsettled: [[Int]] = []
        for col in 1..<grid[0].count {
            if "ABCD".contains(grid[1][col]) {
                unsettled.append([1, col])
            }
        }
        for col in [3, 5, 7, 9] {
            var roomFullFromBack = true
            for row in (2...grid.count - 2).reversed() {
                let coord = [row, col]
                let wantChar = roomCoordToWantChar[coord]!
                let gotChar = grid[row][col]
                if gotChar != "." {
                    if gotChar != wantChar {
                        roomFullFromBack = false
                        unsettled.append(coord)
                    } else if gotChar == wantChar && !roomFullFromBack {
                        unsettled.append(coord)
                    }
                }
            }
        }
        return unsettled
    }
    
    func getNextPossibleMoves(unsettledCoord: [Int], roomCoordToWantChar: [[Int]: String]) -> [[Int]] {
        let unsettledChar = grid[unsettledCoord[0]][unsettledCoord[1]]
        var possible: [[Int]] = []
        let startedInHallway = unsettledCoord[0] == 1
        var queue: [[Int]] = [unsettledCoord]
        var seen: Set<[Int]> = []
        
        let coordsInFrontOfRooms: Set<[Int]> = [[1, 3], [1, 5], [1, 7], [1, 9]]
        
        while !queue.isEmpty {
            let front = queue.removeFirst()
            if seen.contains(front) { continue }
            seen.insert(front)
            
            if front != unsettledCoord {
                if !coordsInFrontOfRooms.contains(front) {
                    if let wantChar = roomCoordToWantChar[front] {
                        if wantChar == unsettledChar {
                            var isStuckAmphipod = false
                            var roomHasDeeperOpenSpaces = false
                            for r in front[0] + 1..<grid.count - 1 {
                                let char = grid[r][front[1]]
                                if char == "." {
                                    roomHasDeeperOpenSpaces = true
                                }
                                if char != "." && char != unsettledChar {
                                    isStuckAmphipod = true
                                    break
                                }
                            }
                            if !roomHasDeeperOpenSpaces && !isStuckAmphipod {
                                possible.append(front)
                            }
                        }
                    } else if !startedInHallway {
                        possible.append(front)
                    }
                }
            }
            
            for d in [[-1, 0], [1, 0], [0, -1], [0, 1]] {
                let next = [front[0] + d[0], front[1] + d[1]]
                if grid[next[0]][next[1]] == "." {
                    queue.append(next)
                }
            }
        }
        return possible
    }
}

func calcEnergy(char: String, start: [Int], end: [Int]) -> Int {
    var dist = abs(end[1] - start[1])
    dist += start[0] - 1
    dist += end[0] - 1
    let energyPerType: [String: Int] = ["A": 1, "B": 10, "C": 100, "D": 1000]
    return energyPerType[char]! * dist
}

func amphipod(input: String) -> Int {
    func parseInput(input: String) -> State {
        let grid = input.split(separator: "\n").map { $0.map { String($0) } }
        return State(grid: grid, energyUsed: 0, path: "")
    }
    
    var start = parseInput(input: input)
    let roomCoordToWantChar: [[Int]: String] = [
        [2, 3]: "A", [3, 3]: "A", [4, 3]: "A", [5, 3]: "A",
        [2, 5]: "B", [3, 5]: "B", [4, 5]: "B", [5, 5]: "B",
        [2, 7]: "C", [3, 7]: "C", [4, 7]: "C", [5, 7]: "C",
        [2, 9]: "D", [3, 9]: "D", [4, 9]: "D", [5, 9]: "D",
    ]
    
    start.grid.append(Array(repeating: "", count: start.grid[0].count))
    start.grid.append(Array(repeating: "", count: start.grid[0].count))
    start.grid[6] = start.grid[4]
    start.grid[5] = start.grid[3]
    start.grid[3] = "  #D#C#B#A#  ".map { String($0) }
    start.grid[4] = "  #D#B#A#C#  ".map { String($0) }
    
    var minHeap = MinHeap()
    minHeap.add(start)
    var seenGrids: Set<String> = []
    
    while minHeap.length() > 0 {
        let front = minHeap.remove() as! State
        let key = front.grid.flatMap { $0 }.joined()
        if seenGrids.contains(key) { continue }
        seenGrids.insert(key)
        
        if front.allDone(roomCoordToWantChar: roomCoordToWantChar) {
            return front.energyUsed
        }
        
        let unsettledCoords = front.getUnsettledCoords(roomCoordToWantChar: roomCoordToWantChar)
        for unsettledCoord in unsettledCoords {
            let ur = unsettledCoord[0], uc = unsettledCoord[1]
            let nextMoves = front.getNextPossibleMoves(unsettledCoord: unsettledCoord, roomCoordToWantChar: roomCoordToWantChar)
            for nextCoord in nextMoves {
                let nr = nextCoord[0], nc = nextCoord[1]
                
                var cp = front.copy()
                cp.energyUsed += calcEnergy(char: cp.grid[ur][uc], start: unsettledCoord, end: nextCoord)
                cp.path += "\(front.grid[ur][uc])\(unsettledCoord)->\(nextCoord){\(cp.energyUsed)},"
                cp.grid[nr][nc] = cp.grid[ur][uc]
                cp.grid[ur][uc] = "."
                
                minHeap.add(cp)
            }
        }
    }
    
    fatalError("should return from loop")
}

let input = try String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
let ans = amphipod(input: input)
print(ans)
