
import Foundation

protocol HeapNode {
    func value() -> Int
}

class MinHeap {
    private var nodes: [HeapNode] = []
    
    func front() -> HeapNode? {
        return nodes.first
    }
    
    func add(_ newNode: HeapNode) {
        nodes.append(newNode)
        heapifyFromEnd()
    }
    
    func remove() -> HeapNode? {
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
    
    private func heapifyFromEnd() {
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
    
    private func heapifyFromStart() {
        var currentIndex = 0
        while true {
            var smallerChildIndex = currentIndex
            for i in 1...2 {
                let childIndex = currentIndex * 2 + i
                if childIndex < nodes.count && nodes[childIndex].value() < nodes[smallerChildIndex].value() {
                    smallerChildIndex = childIndex
                }
            }
            if smallerChildIndex == currentIndex {
                return
            }
            nodes.swapAt(smallerChildIndex, currentIndex)
            currentIndex = smallerChildIndex
        }
    }
}

struct State: Hashable, HeapNode {
    var grid: [[String]]
    var energyUsed: Int
    var path: String
    
    func value() -> Int {
        return energyUsed
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(grid.description)
    }
    
    static func == (lhs: State, rhs: State) -> Bool {
        return lhs.grid == rhs.grid
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
        
        while !queue.isEmpty {
            let front = queue.removeFirst()
            if seen.contains(front) {
                continue
            }
            seen.insert(front)
            
            if front != unsettledCoord {
                if ![ [1, 3], [1, 5], [1, 7], [1, 9] ].contains(front) {
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
            
            for d in [ [-1, 0], [1, 0], [0, -1], [0, 1] ] {
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
    let start = parseInput(input: input)
    let roomCoordToWantChar: [[Int]: String] = [
        [2, 3]: "A", [3, 3]: "A",
        [2, 5]: "B", [3, 5]: "B",
        [2, 7]: "C", [3, 7]: "C",
        [2, 9]: "D", [3, 9]: "D",
    ]
    let minHeap = MinHeap()
    minHeap.add(start)
    var seenGrids: Set<State> = []
    
    while minHeap.length() > 0 {
        let front = minHeap.remove() as! State
        if seenGrids.contains(front) {
            continue
        }
        seenGrids.insert(front)
        
        if front.allDone(roomCoordToWantChar: roomCoordToWantChar) {
            return front.energyUsed
        }
        
        let unsettledCoords = front.getUnsettledCoords(roomCoordToWantChar: roomCoordToWantChar)
        for unsettledCoord in unsettledCoords {
            let nextMoves = front.getNextPossibleMoves(unsettledCoord: unsettledCoord, roomCoordToWantChar: roomCoordToWantChar)
            for nextCoord in nextMoves {
                var cp = front
                cp.energyUsed += calcEnergy(char: cp.grid[unsettledCoord[0]][unsettledCoord[1]], start: unsettledCoord, end: nextCoord)
                cp.path += "\(front.grid[unsettledCoord[0]][unsettledCoord[1]])\(unsettledCoord)->\(nextCoord){\(cp.energyUsed)},"
                cp.grid[nextCoord[0]][nextCoord[1]] = cp.grid[unsettledCoord[0]][unsettledCoord[1]]
                cp.grid[unsettledCoord[0]][unsettledCoord[1]] = "."
                minHeap.add(cp)
            }
        }
    }
    fatalError("should return from loop")
}

func parseInput(input: String) -> State {
    var grid: [[String]] = []
    for line in input.split(separator: "\n") {
        grid.append(line.map { String($0) })
    }
    return State(grid: grid, energyUsed: 0, path: "")
}

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL).trimmingCharacters(in: .whitespacesAndNewlines)
let ans = amphipod(input: input)
print(ans)
