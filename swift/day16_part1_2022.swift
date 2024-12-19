
import Foundation

struct Valve {
    let id: String
    let flow: Int
    var tunnels: [String: Int]
}

func readInput(from file: String) -> String {
    guard let content = try? String(contentsOfFile: file) else {
        fatalError("Failed to read input file")
    }
    return content.trimmingCharacters(in: .whitespacesAndNewlines)
}

func parseInput(from input: String) -> [String: Valve] {
    var valves: [String: Valve] = [:]
    for line in input.components(separatedBy: "\n") {
        let parts = line.components(separatedBy: "; ")
        let id = String(parts[0].split(separator: " ")[1])
        let flow = Int(parts[0].split(separator: "=")[1])!
        let tunnelString = parts[1].replacingOccurrences(of: "tunnels lead to valves ", with: "").replacingOccurrences(of: "tunnel leads to valve ", with: "")
        let tunnels = tunnelString.components(separatedBy: ", ").reduce(into: [String: Int]()) { $0[$1] = 1 }
        valves[id] = Valve(id: id, flow: flow, tunnels: [id: 0].merging(tunnels, uniquingKeysWith: { $1 }))
    }
    return valves
}

func floydWarshall(valves: inout [String: Valve]) {
    for k in valves.keys {
        for i in valves.keys {
            for j in valves.keys {
                if let dik = valves[i]?.tunnels[k], let dkj = valves[k]?.tunnels[j] {
                    if let dij = valves[i]?.tunnels[j], dij > dik + dkj {
                        valves[i]?.tunnels[j] = dik + dkj
                    } else if valves[i]?.tunnels[j] == nil {
                        valves[i]?.tunnels[j] = dik + dkj
                    }
                }
            }
        }
    }
}

func maxPressure(valves: [String: Valve], curr: String, minute: Int, pressure: Int, open: [String], cache: inout [String: Int]) -> Int {
    let cacheKey = "\(curr)-\(minute)-\(open.sorted().joined(separator: ","))"
    if let cached = cache[cacheKey] {
        return cached
    }
    
    var maxVal = pressure
    for next in open {
        let newOpen = open.filter { $0 != next }
        let timeLeft = minute - valves[curr]!.tunnels[next]! - 1
        if timeLeft > 0 {
            let newPressure = timeLeft * valves[next]!.flow + pressure
            maxVal = max(maxVal, maxPressure(valves: valves, curr: next, minute: timeLeft, pressure: newPressure, open: newOpen, cache: &cache))
        }
    }
    
    cache[cacheKey] = maxVal
    return maxVal
}

var valves = parseInput(from: readInput(from: "input.txt"))
floydWarshall(valves: &valves)
let openValves = valves.values.filter { $0.flow > 0 }.map { $0.id }
var cache: [String: Int] = [:]
print(maxPressure(valves: valves, curr: "AA", minute: 30, pressure: 0, open: openValves, cache: &cache))
