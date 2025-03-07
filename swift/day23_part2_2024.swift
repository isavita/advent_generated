
import Foundation

func solve() {
    var graph: [String: Set<String>] = [:]
    var nodes: Set<String> = []

    do {
        let data = try String(contentsOfFile: "input.txt", encoding: .utf8)
        let lines = data.components(separatedBy: .newlines)
        for line in lines {
            if line.isEmpty { continue }
            let parts = line.components(separatedBy: "-")
            let a = parts[0]
            let b = parts[1]
            graph[a, default: []].insert(b)
            graph[b, default: []].insert(a)
            nodes.insert(a)
            nodes.insert(b)
        }
    } catch {
        print("Error reading file: \(error)")
        return
    }

    var bestClique: [String] = []

    func bronKerbosch(r: inout [String], p: inout [String], x: inout [String]) {
        if p.isEmpty && x.isEmpty {
            if r.count > bestClique.count {
                bestClique = r
            }
            return
        }

        let pCopy = p
        for v in pCopy {
            var neighbors = Array(graph[v] ?? [])
            var newR = r
            newR.append(v)
            var newP = p.filter { neighbors.contains($0) }
            var newX = x.filter { neighbors.contains($0) }
            bronKerbosch(r: &newR, p: &newP, x: &newX)
            if let index = p.firstIndex(of: v) {
                p.remove(at: index)
            }
            x.append(v)
        }
    }
    
    var r: [String] = []
    var p = Array(nodes)
    var x: [String] = []

    bronKerbosch(r: &r, p: &p, x: &x)
    bestClique.sort()
    print(bestClique.joined(separator: ","))
}

solve()
