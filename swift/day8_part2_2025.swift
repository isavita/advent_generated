import Foundation

struct Point {
    let id: Int
    let x: Int
    let y: Int
    let z: Int
}

struct Edge {
    let u: Int
    let v: Int
    let distSq: Int
}

final class UnionFind {
    private var parent: [Int]
    var components: Int

    init(_ n: Int) {
        parent = Array(0..<n)
        components = n
    }

    func find(_ i: Int) -> Int {
        if parent[i] != i {
            parent[i] = find(parent[i])
        }
        return parent[i]
    }

    func union(_ a: Int, _ b: Int) -> Bool {
        let ra = find(a)
        let rb = find(b)
        if ra == rb { return false }
        parent[rb] = ra
        components -= 1
        return true
    }
}

func distSq(_ p1: Point, _ p2: Point) -> Int {
    let dx = p1.x - p2.x
    let dy = p1.y - p2.y
    let dz = p1.z - p2.z
    return dx*dx + dy*dy + dz*dz
}

func main() {
    guard let data = try? String(contentsOfFile: "input.txt", encoding: .utf8) else { return }
    var points = [Point]()
    var id = 0
    for line in data.split(separator: "\n") {
        let trimmed = line.trimmingCharacters(in: .whitespacesAndNewlines)
        if trimmed.isEmpty { continue }
        let parts = trimmed.split(separator: ",")
        if parts.count != 3 { continue }
        let x = Int(parts[0]) ?? 0
        let y = Int(parts[1]) ?? 0
        let z = Int(parts[2]) ?? 0
        points.append(Point(id: id, x: x, y: y, z: z))
        id += 1
    }

    let n = points.count
    if n < 2 { return }

    var edges = [Edge]()
    edges.reserveCapacity(n * (n - 1) / 2)
    for i in 0..<n {
        for j in i+1..<n {
            edges.append(Edge(u: i, v: j, distSq: distSq(points[i], points[j])))
        }
    }

    edges.sort { $0.distSq < $1.distSq }

    let uf = UnionFind(n)

    for e in edges {
        if uf.union(e.u, e.v) && uf.components == 1 {
            let p1 = points[e.u]
            let p2 = points[e.v]
            print("Connected \(p1.x),\(p1.y),\(p1.z) and \(p2.x),\(p2.y),\(p2.z)")
            let result = p1.x * p2.x
            print("Product of X coordinates: \(result)")
            break
        }
    }
}

main()
