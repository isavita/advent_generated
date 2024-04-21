import Foundation

typealias Vertice = String
struct Edge: Hashable {
    let start: Vertice
    let end: Vertice
    let weight: Int
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(start)
        hasher.combine(end)
        hasher.combine(weight)
    }
    
    static func == (lhs: Edge, rhs: Edge) -> Bool {
        return lhs.start == rhs.start && lhs.end == rhs.end && lhs.weight == rhs.weight
    }
}

typealias Graph = [Vertice: [Edge: Bool]]

func parseInput(_ input: [String]) -> Graph {
    var graph: Graph = [:]
    var weight = 1
    
    for line in input {
        let parts = line.components(separatedBy: ": ")
        let vertice = parts[0]
        let others = parts[1].components(separatedBy: " ")
        
        if graph[vertice] == nil {
            graph[vertice] = [:]
        }
        
        for other in others {
            let otherVertice = other
            if graph[otherVertice] == nil {
                graph[otherVertice] = [:]
            }
            
            graph[vertice]![Edge(start: vertice, end: otherVertice, weight: weight)] = true
            graph[otherVertice]![Edge(start: otherVertice, end: vertice, weight: weight)] = true
        }
    }
    
    return graph
}

func breadthFirstSearch(_ graph: Graph, start: Vertice, goalFunc: (Vertice) -> Bool) -> (Bool, [Vertice: Vertice]) {
    var frontier: [Vertice] = [start]
    var reached: [Vertice: Bool] = [start: true]
    var cameFrom: [Vertice: Vertice] = [start: start]
    
    while !frontier.isEmpty {
        let current = frontier.removeFirst()
        
        if goalFunc(current) {
            return (true, cameFrom)
        }
        
        for edge in graph[current]!.keys {
            if reached[edge.end] == nil {
                frontier.append(edge.end)
                reached[edge.end] = true
                cameFrom[edge.end] = current
            }
        }
    }
    
    return (false, cameFrom)
}

func reconstructPath(_ start: Vertice, _ end: Vertice, _ cameFrom: [Vertice: Vertice]) -> [Vertice] {
    var path: [Vertice] = []
    var current = end
    
    while current != start {
        path.insert(current, at: 0)
        current = cameFrom[current]!
    }
    
    path.insert(start, at: 0)
    return path
}

func copyGraph(_ graph: Graph) -> Graph {
    var newGraph: Graph = [:]
    
    for (vertice, edges) in graph {
        newGraph[vertice] = [:]
        for edge in edges.keys {
            newGraph[vertice]![edge] = true
        }
    }
    
    return newGraph
}

func solve(_ input: [String]) -> Int {
    let minCut = 3
    
    let graph = parseInput(input)
    
    var source: Vertice!
    for vertice in graph.keys {
        source = vertice
        break
    }
    
    var separateGraph: Graph!
    
    for end in graph.keys {
        if source == end {
            continue
        }
        
        var newGraph = copyGraph(graph)
        
        for _ in 0..<minCut {
            let (_, cameFrom) = breadthFirstSearch(newGraph, start: source, goalFunc: { $0 == end })
            let path = reconstructPath(source, end, cameFrom)
            
            for i in 0..<path.count-1 {
                let edge = Edge(start: path[i], end: path[i+1], weight: 1)
                newGraph[path[i]]![edge] = nil
            }
        }
        
        let (isValid, _) = breadthFirstSearch(newGraph, start: source, goalFunc: { $0 == end })
        if !isValid {
            separateGraph = newGraph
            break
        }
    }
    
    let (_, cameFrom) = breadthFirstSearch(separateGraph, start: source, goalFunc: { _ in false })
    let length1 = cameFrom.count
    let length2 = separateGraph.count - length1
    
    return length1 * length2
}

func readFile(_ fileName: String) -> [String] {
    do {
        let fileContent = try String(contentsOfFile: fileName, encoding: .utf8)
        return fileContent.components(separatedBy: "\n")
    } catch {
        fatalError("Error reading file")
    }
}

let input = readFile("input.txt")
print(solve(input))