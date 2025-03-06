
import Foundation

// Define a struct to represent a position in the grid, including direction and consecutive steps.
struct State: Hashable, Equatable {
    let row: Int
    let col: Int
    let direction: Direction
    let steps: Int

    enum Direction: Int, Hashable {
        case up = 0, right = 1, down = 2, left = 3

        // Returns the possible directions after a move (no reverse).
        func nextDirections() -> [Direction] {
            switch self {
            case .up, .down:
                return [.left, .right]
            case .left, .right:
                return [.up, .down]
            }
        }
    }
}

// Function to solve the puzzle.
func solve(grid: [[Int]]) -> Int {
    let rows = grid.count
    let cols = grid[0].count
    
    // Priority queue for Dijkstra's algorithm.
    var priorityQueue = PriorityQueue<(State, Int)> { $0.1 < $1.1 }
    
    // Initialize distances with infinity.
    var distances: [State: Int] = [:]
    
    // Starting states (can start moving right or down).
    let startRight = State(row: 0, col: 0, direction: .right, steps: 0)
    let startDown = State(row: 0, col: 0, direction: .down, steps: 0)
    
    priorityQueue.enqueue((startRight, 0))
    priorityQueue.enqueue((startDown, 0))
    distances[startRight] = 0
    distances[startDown] = 0

    while let (currentState, currentCost) = priorityQueue.dequeue() {
        
        // If we reached the destination, return the cost.
        if currentState.row == rows - 1 && currentState.col == cols - 1 {
            return currentCost
        }

        // Optimization: Skip if a shorter path to this state has already been found
        if currentCost > distances[currentState, default: Int.max] {
            continue
        }

        // Explore next possible moves:
        
        // 1. Continue in the same direction (if allowed).
        if currentState.steps < 3 {
            let nextRow = currentState.row + (currentState.direction == .down ? 1 : (currentState.direction == .up ? -1 : 0))
            let nextCol = currentState.col + (currentState.direction == .right ? 1 : (currentState.direction == .left ? -1 : 0))
            
            if nextRow >= 0 && nextRow < rows && nextCol >= 0 && nextCol < cols {
                let nextState = State(row: nextRow, col: nextCol, direction: currentState.direction, steps: currentState.steps + 1)
                let nextCost = currentCost + grid[nextRow][nextCol]
                
                if nextCost < distances[nextState, default: Int.max] {
                    distances[nextState] = nextCost
                    priorityQueue.enqueue((nextState, nextCost))
                }
            }
        }
        
        // 2. Turn (change direction).
        for nextDirection in currentState.direction.nextDirections() {
            let nextRow = currentState.row + (nextDirection == .down ? 1 : (nextDirection == .up ? -1 : 0))
            let nextCol = currentState.col + (nextDirection == .right ? 1 : (nextDirection == .left ? -1 : 0))
            
            if nextRow >= 0 && nextRow < rows && nextCol >= 0 && nextCol < cols {
                let nextState = State(row: nextRow, col: nextCol, direction: nextDirection, steps: 1) // Reset steps to 1 after turning
                let nextCost = currentCost + grid[nextRow][nextCol]
                
                if nextCost < distances[nextState, default: Int.max] {
                    distances[nextState] = nextCost
                    priorityQueue.enqueue((nextState, nextCost))
                }
            }
        }
    }

    return -1 // Should never reach here if a path exists.
}

// Priority Queue implementation (Min Heap).
struct PriorityQueue<Element> {
    var elements: [Element]
    let priorityFunction: (Element, Element) -> Bool
    
    init(priorityFunction: @escaping (Element, Element) -> Bool) {
        self.elements = []
        self.priorityFunction = priorityFunction
    }
    
    mutating func enqueue(_ element: Element) {
        elements.append(element)
        swim(elements.count - 1)
    }
    
    mutating func dequeue() -> Element? {
        guard !elements.isEmpty else { return nil }
        if elements.count == 1 {
            return elements.removeFirst()
        }
        elements.swapAt(0, elements.count - 1)
        let element = elements.removeLast()
        sink(0)
        return element
    }
    
    private mutating func swim(_ index: Int) {
        var index = index
        while index > 0 {
            let parentIndex = (index - 1) / 2
            if priorityFunction(elements[index], elements[parentIndex]) {
                elements.swapAt(index, parentIndex)
                index = parentIndex
            } else {
                break
            }
        }
    }
    
    private mutating func sink(_ index: Int) {
        var index = index
        while true {
            let leftChildIndex = 2 * index + 1
            let rightChildIndex = 2 * index + 2
            var candidateIndex = index
            
            if leftChildIndex < elements.count && priorityFunction(elements[leftChildIndex], elements[candidateIndex]) {
                candidateIndex = leftChildIndex
            }
            if rightChildIndex < elements.count && priorityFunction(elements[rightChildIndex], elements[candidateIndex]) {
                candidateIndex = rightChildIndex
            }
            
            if candidateIndex != index {
                elements.swapAt(index, candidateIndex)
                index = candidateIndex
            } else {
                break
            }
        }
    }
}


// Main function to read input and call the solver.
func main() {
    do {
        // Read the input from the file.
        let input = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let lines = input.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: "\n")
        let grid = lines.map { $0.map { Int(String($0))! } }

        // Solve the puzzle and print the result.
        let result = solve(grid: grid)
        print(result)

    } catch {
        print("Error reading file: \(error)")
    }
}

// Call the main function to start the program.
main()

