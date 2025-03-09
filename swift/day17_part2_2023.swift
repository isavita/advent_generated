import Foundation

// Priority Queue implementation
struct IntPriorityQueueItem<T> {
    let item: T
    let priority: Int
}

struct IntPriorityQueue<T> {
    private var items = [IntPriorityQueueItem<T>]()
    
    var isEmpty: Bool {
        return items.isEmpty
    }
    
    var count: Int {
        return items.count
    }
    
    mutating func push(_ item: T, priority: Int) {
        let queueItem = IntPriorityQueueItem(item: item, priority: priority)
        items.append(queueItem)
        siftUp(from: items.count - 1)
    }
    
    mutating func pop() -> IntPriorityQueueItem<T>? {
        guard !items.isEmpty else { return nil }
        
        let firstItem = items[0]
        items[0] = items[items.count - 1]
        items.removeLast()
        
        if !items.isEmpty {
            siftDown(from: 0)
        }
        
        return firstItem
    }
    
    private mutating func siftUp(from index: Int) {
        var childIndex = index
        let child = items[childIndex]
        
        while childIndex > 0 {
            let parentIndex = (childIndex - 1) / 2
            if items[parentIndex].priority <= child.priority {
                break
            }
            
            items[childIndex] = items[parentIndex]
            childIndex = parentIndex
        }
        
        items[childIndex] = child
    }
    
    private mutating func siftDown(from index: Int) {
        var parentIndex = index
        let parent = items[parentIndex]
        
        while true {
            let leftChildIndex = 2 * parentIndex + 1
            let rightChildIndex = leftChildIndex + 1
            
            guard leftChildIndex < items.count else { break }
            
            var minChildIndex = leftChildIndex
            if rightChildIndex < items.count && items[rightChildIndex].priority < items[leftChildIndex].priority {
                minChildIndex = rightChildIndex
            }
            
            if parent.priority <= items[minChildIndex].priority {
                break
            }
            
            items[parentIndex] = items[minChildIndex]
            parentIndex = minChildIndex
        }
        
        items[parentIndex] = parent
    }
}

// Coordinate structure
struct Coord: Hashable {
    let x: Int
    let y: Int
    
    func add(_ other: Coord) -> Coord {
        return Coord(x: self.x + other.x, y: self.y + other.y)
    }
    
    func subtract(_ other: Coord) -> Coord {
        return Coord(x: self.x - other.x, y: self.y - other.y)
    }
    
    func opposite() -> Coord {
        return Coord(x: -self.x, y: -self.y)
    }
}

// Grid structure
struct Grid {
    let width: Int
    let height: Int
    let data: [Coord: Int]
    
    static let north = Coord(x: 0, y: -1)
    static let west = Coord(x: -1, y: 0)
    static let south = Coord(x: 0, y: 1)
    static let east = Coord(x: 1, y: 0)
    
    func isInBounds(_ coord: Coord) -> Bool {
        return 0 <= coord.x && coord.x < width && 0 <= coord.y && coord.y < height
    }
    
    func neighbors4(_ coord: Coord) -> [Coord] {
        let directions = [Grid.north, Grid.west, Grid.south, Grid.east]
        var neighbors = [Coord]()
        
        for dir in directions {
            let neighbor = coord.add(dir)
            if isInBounds(neighbor) {
                neighbors.append(neighbor)
            }
        }
        
        return neighbors
    }
    
    func dijkstra(start: Coord, goal: Coord) -> ([Coord: Coord], [Coord: Int]) {
        var frontier = IntPriorityQueue<Coord>()
        frontier.push(start, priority: 0)
        
        var cameFrom = [Coord: Coord]()
        var costSoFar = [Coord: Int]()
        cameFrom[start] = start
        costSoFar[start] = 0
        
        while !frontier.isEmpty {
            guard let minItem = frontier.pop() else { break }
            let current = minItem.item
            let currentCost = minItem.priority
            
            if current == goal {
                break
            }
            
            for next in neighbors4(current) {
                guard let nextCost = data[next] else { continue }
                let newCost = currentCost + nextCost
                
                if costSoFar[next] == nil || newCost < costSoFar[next]! {
                    costSoFar[next] = newCost
                    let priority = newCost
                    frontier.push(next, priority: priority)
                    cameFrom[next] = current
                }
            }
        }
        
        return (cameFrom, costSoFar)
    }
    
    func heuristic(_ c1: Coord, _ c2: Coord) -> Int {
        return abs(c1.x - c2.x) + abs(c1.y - c2.y)
    }
    
    func aStarConstrained(start: Coord, goal: Coord, minStraight: Int, maxStraight: Int) -> Int {
        struct Info: Hashable {
            let coord: Coord
            let dir: Coord
            let numStraight: Int
        }
        
        let startInfo = Info(coord: start, dir: Coord(x: 0, y: 0), numStraight: 0)
        
        var frontier = IntPriorityQueue<Info>()
        frontier.push(startInfo, priority: 0)
        
        var cameFrom = [Info: Info]()
        var costSoFar = [Info: Int]()
        cameFrom[startInfo] = startInfo
        costSoFar[startInfo] = 0
        
        while !frontier.isEmpty {
            guard let minItem = frontier.pop() else { break }
            let current = minItem.item
            let currentCost = costSoFar[current]!
            
            if current.coord == goal {
                return currentCost
            }
            
            for next in neighbors4(current.coord) {
                let newDir = next.subtract(current.coord)
                var newNumStraight = 1
                if newDir == current.dir {
                    newNumStraight += current.numStraight
                }
                
                let nextInfo = Info(
                    coord: next,
                    dir: newDir,
                    numStraight: newNumStraight
                )
                
                guard let nextCost = data[next] else { continue }
                let newCost = currentCost + nextCost
                let actualCost = costSoFar[nextInfo]
                
                let isLowerCost = actualCost == nil || newCost < actualCost!
                let isValidStraight = (current.numStraight >= minStraight || newDir == current.dir || current.coord == start) &&
                    (newNumStraight <= maxStraight)
                let isNotOppositeDirection = newDir != current.dir.opposite()
                
                let isValid = isLowerCost && isValidStraight && isNotOppositeDirection
                if isValid {
                    costSoFar[nextInfo] = newCost
                    cameFrom[nextInfo] = current
                    
                    let priority = newCost + heuristic(next, goal)
                    frontier.push(nextInfo, priority: priority)
                }
            }
        }
        
        return -1
    }
}

func buildGrid(input: [String]) -> Grid {
    let width = input[0].count
    let height = input.count
    var data = [Coord: Int]()
    
    for (y, line) in input.enumerated() {
        for (x, char) in line.enumerated() {
            if let value = char.wholeNumberValue {
                data[Coord(x: x, y: y)] = value
            }
        }
    }
    
    return Grid(width: width, height: height, data: data)
}

func solve(input: [String]) -> Int {
    let grid = buildGrid(input: input)
    
    let start = Coord(x: 0, y: 0)
    let goal = Coord(x: grid.width - 1, y: grid.height - 1)
    let result = grid.aStarConstrained(start: start, goal: goal, minStraight: 4, maxStraight: 10)
    
    return result
}

func readFile(_ fileName: String) throws -> [String] {
    let fileURL = URL(fileURLWithPath: fileName)
    let content = try String(contentsOf: fileURL, encoding: .utf8)
    return content.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: "\n")
}

// Main function
do {
    let input = try readFile("input.txt")
    print(solve(input: input))
} catch {
    print("Error reading file: \(error)")
}
