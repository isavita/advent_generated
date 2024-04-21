import Foundation

class Node {
    let name: String
    var children: [Node] = []
    var parent: Node?

    init(name: String) {
        self.name = name
    }
}

func findOrCreateNode(name: String, nodes: inout [String: Node]) -> Node {
    if let node = nodes[name] {
        return node
    }
    let node = Node(name: name)
    nodes[name] = node
    return node
}

func buildOrbitMap(from file: String) -> [String: Node] {
    var nodes: [String: Node] = [:]
    do {
        let contents = try String(contentsOfFile: file, encoding: .utf8)
        let lines = contents.components(separatedBy: "\n")
        for line in lines {
            let parts = line.components(separatedBy: ")")
            let center = findOrCreateNode(name: String(parts[0].trimmingCharacters(in: .whitespaces)), nodes: &nodes)
            let orbiter = findOrCreateNode(name: String(parts[1].trimmingCharacters(in: .whitespaces)), nodes: &nodes)
            center.children.append(orbiter)
            orbiter.parent = center
        }
    } catch {
        print("Error reading file: \(error)")
        return [:]
    }
    return nodes
}

func pathToRoot(_ node: Node) -> [Node] {
    var path: [Node] = []
    var current: Node? = node
    while let node = current {
        path.append(node)
        current = node.parent
    }
    return path
}

func findCommonAncestor(_ node1: Node, _ node2: Node) -> (Int, Int) {
    let path1 = pathToRoot(node1)
    let path2 = pathToRoot(node2)

    var i = path1.count - 1
    var j = path2.count - 1

    while i >= 0 && j >= 0 && path1[i] === path2[j] {
        i -= 1
        j -= 1
    }
    return (i + 1, j + 1)
}

let orbitMap = buildOrbitMap(from: "input.txt")
if let you = orbitMap["YOU"]?.parent, let san = orbitMap["SAN"]?.parent {
    let (transfersYOU, transfersSAN) = findCommonAncestor(you, san)
    print(transfersYOU + transfersSAN)
}