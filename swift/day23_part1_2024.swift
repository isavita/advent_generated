
import Foundation

// Function to read the connections from the input file
func readConnections(from filePath: String) -> [String: Set<String>] {
    var connections: [String: Set<String>] = [:]
    
    do {
        let fileContent = try String(contentsOfFile: filePath, encoding: .utf8)
        let lines = fileContent.components(separatedBy: .newlines).filter { !$0.isEmpty }
        
        for line in lines {
            let computers = line.components(separatedBy: "-")
            guard computers.count == 2 else {
                print("Invalid connection format: \(line)")
                continue
            }
            let computer1 = computers[0]
            let computer2 = computers[1]
            
            connections[computer1, default: []].insert(computer2)
            connections[computer2, default: []].insert(computer1)
        }
        
    } catch {
        print("Error reading file: \(error)")
    }
    
    return connections
}

// Function to find all sets of three interconnected computers
func findInterconnectedTriplets(connections: [String: Set<String>]) -> [[String]] {
    var triplets: [[String]] = []
    let computers = Array(connections.keys)
    
    for i in 0..<computers.count {
        for j in i+1..<computers.count {
            for k in j+1..<computers.count {
                let comp1 = computers[i]
                let comp2 = computers[j]
                let comp3 = computers[k]
                
                if connections[comp1, default:[]].contains(comp2) &&
                    connections[comp1, default:[]].contains(comp3) &&
                    connections[comp2, default:[]].contains(comp3) {
                    triplets.append([comp1, comp2, comp3].sorted()) // Sort to avoid duplicate sets
                }
            }
        }
    }
    
    return triplets
}

// Function to filter triplets containing at least one computer starting with 't'
func filterTripletsWithT(triplets: [[String]]) -> Int {
    var count = 0
    for triplet in triplets {
        if triplet.contains(where: { $0.starts(with: "t") }) {
            count += 1
        }
    }
    return count
}

// MARK: - Main Execution

func main() {
    let filePath = "input.txt"
    
    let connections = readConnections(from: filePath)
    let triplets = findInterconnectedTriplets(connections: connections)
    let count = filterTripletsWithT(triplets: triplets)
    
    print(count)
}

// Ensure this script has a proper entry point
main()
