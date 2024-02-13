
import Foundation

struct Node {
    var used: Int
    var avail: Int
}

func readNodes(from file: String) -> [Node] {
    guard let content = try? String(contentsOfFile: file) else { return [] }
    
    let nodeRegex = try! NSRegularExpression(pattern: "node-x\\d+-y\\d+\\s+\\d+T\\s+(\\d+)T\\s+(\\d+)T\\s+\\d+%", options: [])
    let lines = content.components(separatedBy: .newlines)
    
    var nodes: [Node] = []
    for line in lines {
        let matches = nodeRegex.matches(in: line, options: [], range: NSRange(location: 0, length: line.utf16.count))
        if let match = matches.first {
            let usedRange = Range(match.range(at: 1), in: line)!
            let availRange = Range(match.range(at: 2), in: line)!
            let used = Int(line[usedRange])!
            let avail = Int(line[availRange])!
            nodes.append(Node(used: used, avail: avail))
        }
    }
    
    return nodes
}

func countViablePairs(nodes: [Node]) -> Int {
    var count = 0
    for i in 0..<nodes.count {
        for j in 0..<nodes.count {
            if i != j && nodes[i].used > 0 && nodes[i].used <= nodes[j].avail {
                count += 1
            }
        }
    }
    return count
}

let nodes = readNodes(from: "input.txt")
let viablePairs = countViablePairs(nodes: nodes)
print(viablePairs)
