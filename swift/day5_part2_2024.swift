
import Foundation

func main() {
    // Read input from file
    guard let input = try? String(contentsOfFile: "input.txt") else {
        fatalError("Could not read input file")
    }

    // Split input into rules and updates
    let parts = input.components(separatedBy: "\n\n")
    guard parts.count == 2 else {
        fatalError("Invalid input format")
    }

    // Parse rules
    let ruleLines = parts[0].components(separatedBy: .newlines)
    var rules: [Int: [Int]] = [:]
    for line in ruleLines {
        if line.isEmpty { continue }
        let ruleComponents = line.components(separatedBy: "|")
        guard ruleComponents.count == 2,
              let before = Int(ruleComponents[0]),
              let after = Int(ruleComponents[1]) else {
            fatalError("Invalid rule format: \(line)")
        }
        rules[before, default: []].append(after)
    }

    // Parse updates
    let updateLines = parts[1].components(separatedBy: .newlines)
    var updates: [[Int]] = []
    for line in updateLines {
        if line.isEmpty { continue }
        let updateComponents = line.components(separatedBy: ",")
        let update = updateComponents.compactMap { Int($0) }
        updates.append(update)
    }

    // Function to check if an update is in the correct order
    func isCorrectOrder(update: [Int]) -> Bool {
        for i in 0..<update.count {
            for j in (i + 1)..<update.count {
                let before = update[i]
                let after = update[j]
                if let afterPages = rules[before], afterPages.contains(after) {
                    continue // Correct order
                }
                
                // Check for violation
                if let beforePages = rules[after], beforePages.contains(before) {
                    return false
                }
            }
        }
        return true
    }

    func topologicalSort(update: [Int]) -> [Int] {
            var inDegree: [Int: Int] = [:]
            var adj: [Int: [Int]] = [:]
            
            //build the graph and indegree map from the rules that apply to this update
            for u in update{
                inDegree[u] = 0
                adj[u] = []
            }
        
            for u in update {
                if let nexts = rules[u] {
                  for v in nexts {
                      if update.contains(v){
                        adj[u, default: []].append(v)
                        inDegree[v, default:0] += 1
                      }
                   }
                }
            }

            var queue: [Int] = []
            for u in update{
                if inDegree[u] == 0 {
                    queue.append(u)
                }
            }
            
            var sorted: [Int] = []
            while !queue.isEmpty {
                let u = queue.removeFirst()
                sorted.append(u)

                for v in adj[u] ?? [] {
                  inDegree[v]! -= 1
                    if inDegree[v] == 0 {
                        queue.append(v)
                    }
                }
            }

            return sorted
        }


    // Part 1: Calculate sum of middle page numbers of correctly ordered updates
    var sumOfMiddlePagesCorrect = 0
    for update in updates {
        if isCorrectOrder(update: update) {
            let middleIndex = update.count / 2
            sumOfMiddlePagesCorrect += update[middleIndex]
        }
    }
    print("Part 1: \(sumOfMiddlePagesCorrect)")

    // Part 2: Calculate sum of middle page numbers of incorrectly ordered updates after sorting
    var sumOfMiddlePagesIncorrect = 0
    for update in updates {
        if !isCorrectOrder(update: update) {
             let sortedUpdate = topologicalSort(update: update)
            if sortedUpdate.count == update.count{
                let middleIndex = sortedUpdate.count / 2
                sumOfMiddlePagesIncorrect += sortedUpdate[middleIndex]
            }

        }
    }
    print("Part 2: \(sumOfMiddlePagesIncorrect)")
}

// Set up the main function as the entry point
main()

