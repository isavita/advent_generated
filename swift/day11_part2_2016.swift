
import Foundation

struct Half {
    let isChip: Bool
    let material: String
}

struct State {
    var floors: [[Half]]
    var elevatorLevel: Int
    var steps: Int
    
    func isValid() -> Bool {
        for floor in floors {
            var gensSeen = Set<String>()
            for half in floor {
                if !half.isChip {
                    gensSeen.insert(half.material)
                }
            }
            if !gensSeen.isEmpty {
                for half in floor {
                    if half.isChip && !gensSeen.contains(half.material) {
                        return false
                    }
                }
            }
        }
        return true
    }
    
    func isDone() -> Bool {
        return floors[0...2].allSatisfy { $0.isEmpty }
    }
    
    func getMovablePermIndices() -> [[Int]] {
        var permsToMove: [[Int]] = []
        let currentLevel = floors[elevatorLevel]
        
        for i in 0..<currentLevel.count {
            permsToMove.append([i])
            for j in (i + 1)..<currentLevel.count {
                permsToMove.append([i, j])
            }
        }
        return permsToMove
    }
    
    func clone() -> State {
        return State(floors: floors.map { $0 }, elevatorLevel: elevatorLevel, steps: steps)
    }
    
    func getNextStates() -> [State] {
        var futureStates: [State] = []
        let movablePermIndices = getMovablePermIndices()
        let eleDiffs = (elevatorLevel < floors.count - 1 ? [1] : []) + (elevatorLevel > 0 ? [-1] : [])
        
        for eleDiff in eleDiffs {
            for permIndices in movablePermIndices {
                var cl = clone()
                cl.elevatorLevel += eleDiff
                cl.steps += 1
                let oldLevel = elevatorLevel
                let newLevel = cl.elevatorLevel
                
                for index in permIndices {
                    cl.floors[newLevel].append(cl.floors[oldLevel][index])
                }
                
                for index in permIndices.sorted(by: >) {
                    cl.floors[oldLevel].remove(at: index)
                }
                
                if cl.isValid() {
                    futureStates.append(cl)
                }
            }
        }
        return futureStates
    }
    
    func hashKey() -> String {
        var genToIndex = [String: Int]()
        var chipToIndex = [String: Int]()
        
        for (flIndex, floor) in floors.enumerated() {
            for half in floor {
                if half.isChip {
                    chipToIndex[half.material] = flIndex
                } else {
                    genToIndex[half.material] = flIndex
                }
            }
        }
        
        var pairs: [(Int, Int)] = []
        for material in genToIndex.keys {
            pairs.append((genToIndex[material]!, chipToIndex[material]!))
        }
        
        pairs.sort { $0.0 == $1.0 ? $0.1 < $1.1 : $0.0 < $1.0 }
        
        return "\(elevatorLevel)\(pairs)"
    }
}

func newInitialState(input: String) -> State {
    var state = State(floors: Array(repeating: [], count: 4), elevatorLevel: 0, steps: 0)
    
    for (lineIndex, line) in input.split(separator: "\n").enumerated() {
        let parts = line.split(separator: " ").map { $0.trimmingCharacters(in: .punctuationCharacters) }
        
        for i in 0..<parts.count {
            if parts[i] == "generator" {
                let material = String(parts[i - 1])
                state.floors[lineIndex].append(Half(isChip: false, material: material))
            } else if parts[i] == "microchip" {
                let material = String(parts[i - 1]).prefix(while: { $0 != "-" })
                state.floors[lineIndex].append(Half(isChip: true, material: String(material)))
            }
        }
    }
    
    return state
}

func readFile(path: String) -> String {
    let url = URL(fileURLWithPath: path)
    return try! String(contentsOf: url, encoding: .utf8).trimmingCharacters(in: .newlines)
}

func rtgHellDay(input: String) -> Int {
    var currentState = newInitialState(input: input)
    currentState.floors[0].append(contentsOf: [
        Half(isChip: false, material: "elerium"),
        Half(isChip: true, material: "elerium"),
        Half(isChip: false, material: "dilithium"),
        Half(isChip: true, material: "dilithium")
    ])
    
    var queue: [State] = [currentState]
    var prevStates = Set<String>()
    
    while !queue.isEmpty {
        let front = queue.removeFirst()
        
        if front.isDone() {
            return front.steps
        }
        
        let hash = front.hashKey()
        if prevStates.contains(hash) {
            continue
        }
        prevStates.insert(hash)
        
        let nextStates = front.getNextStates()
        queue.append(contentsOf: nextStates)
    }
    
    return -1
}

let input = readFile(path: "./input.txt")
let answer = rtgHellDay(input: input)
print(answer)
