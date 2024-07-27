
import Foundation

struct Point {
    var x, y, z, t: Int
}

func abs(_ x: Int) -> Int {
    return x < 0 ? -x : x
}

func manhattanDistance(_ a: Point, _ b: Point) -> Int {
    return abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z) + abs(a.t - b.t)
}

class UnionFind {
    var parent: [Int]
    
    init(size: Int) {
        parent = Array(0..<size)
    }
    
    func find(_ x: Int) -> Int {
        if parent[x] != x {
            parent[x] = find(parent[x])
        }
        return parent[x]
    }
    
    func union(_ x: Int, _ y: Int) {
        let rootX = find(x)
        let rootY = find(y)
        if rootX != rootY {
            parent[rootX] = rootY
        }
    }
}

func main() {
    let fileURL = URL(fileURLWithPath: "input.txt")
    let content = try! String(contentsOf: fileURL)
    let points = content.split(separator: "\n").map { line -> Point in
        let coords = line.split(separator: ",").map { Int($0)! }
        return Point(x: coords[0], y: coords[1], z: coords[2], t: coords[3])
    }
    
    let uf = UnionFind(size: points.count)
    
    for i in 0..<points.count {
        for j in (i + 1)..<points.count {
            if manhattanDistance(points[i], points[j]) <= 3 {
                uf.union(i, j)
            }
        }
    }
    
    let constellationCount = Set(uf.parent.map { uf.find($0) }).count
    print(constellationCount)
}

main()
