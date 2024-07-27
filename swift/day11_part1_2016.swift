
import Foundation

struct Halves {
    let isChip: Bool
    let material: String
}

struct State {
    var floors: [[Halves]]
    var elevatorLevel: Int
    var steps: Int
    
    func isDone() -> Bool {
        return floors[0...2].allSatisfy { $0.isEmpty }
    }
    
    func isValid() -> Bool {
        for floor in floors {
            var gensSeen = Set<String>()
            for half in floor {
                if !half.isChip {
                    gensSeen.insert(half.material)
                }
            }
            if !gensSeen.isEmpty {
                for half in floor where half.isChip && !gensSeen.contains(half.material) {
                    return false
                }
            }
        }
        return true
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
                
                for index in permIndices.reversed() {
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
        for (flIndex, fl) in floors.enumerated() {
            for half in fl {
                if half.isChip {
                    chipToIndex[half.material] = flIndex
                } else {
                    genToIndex[half.material] = flIndex
                }
            }
        }
        
        var genChipPairs: [(Int, Int)] = []
        for material in genToIndex.keys {
            genChipPairs.append((genToIndex[material]!, chipToIndex[material]!))
        }
        
        genChipPairs.sort { $0.0 == $1.0 ? $0.1 < $1.1 : $0.0 < $1.0 }
        return "\(elevatorLevel)\(genChipPairs)"
    }
}

func newInitialState(input: String) -> State {
    var floors: [[Halves]] = Array(repeating: [], count: 4)
    
    for (lineIndex, line) in input.split(separator: "\n").enumerated() {
        let parts = line.split(separator: " ").map { $0.trimmingCharacters(in: .whitespacesAndNewlines) }
        for i in 0..<parts.count {
            if parts[i].contains("generator") {
                let material = String(parts[i - 1])
                floors[lineIndex].append(Halves(isChip: false, material: material))
            } else if parts[i].contains("microchip") {
                let material = String(parts[i - 1]).split(separator: "-").first.map(String.init) ?? ""
                floors[lineIndex].append(Halves(isChip: true, material: material))
            }
        }
    }
    
    return State(floors: floors, elevatorLevel: 0, steps: 0)
}

func rtgHellDay(input: String) -> Int {
    var queue: [State] = [newInitialState(input: input)]
    var prevStates: Set<String> = []
    
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

func readFile(path: String) -> String {
    return (try? String(contentsOfFile: path)) ?? ""
}

let input = readFile(path: "./input.txt")
let answer = rtgHellDay(input: input)
print(answer)
