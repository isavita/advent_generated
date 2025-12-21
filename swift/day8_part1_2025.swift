
import Foundation

struct Point { let x: Int; let y: Int; let z: Int }
struct Edge { let u: Int; let v: Int; let d: Int }

let content = try String(contentsOfFile: "input.txt")
let lines = content.split(separator: "\n")
var points = [Point]()
for line in lines {
    let parts = line.split(separator: ",")
    if parts.count == 3 {
        let x = Int(parts[0])!
        let y = Int(parts[1])!
        let z = Int(parts[2])!
        points.append(Point(x: x, y: y, z: z))
    }
}
guard points.count >= 2 else { exit(0) }

var edges = [Edge]()
edges.reserveCapacity(points.count * (points.count - 1) / 2)
for i in 0..<points.count {
    for j in i+1..<points.count {
        let dx = points[i].x - points[j].x
        let dy = points[i].y - points[j].y
        let dz = points[i].z - points[j].z
        let d = dx*dx + dy*dy + dz*dz
        edges.append(Edge(u: i, v: j, d: d))
    }
}
edges.sort { $0.d < $1.d }

var parent = Array(0..<points.count)
var size = Array(repeating: 1, count: points.count)

func find(_ i: Int) -> Int {
    var x = i
    while parent[x] != x {
        parent[x] = parent[parent[x]]
        x = parent[x]
    }
    return x
}
func union(_ a: Int, _ b: Int) {
    var ra = find(a), rb = find(b)
    if ra == rb { return }
    if size[ra] < size[rb] { swap(&ra, &rb) }
    parent[rb] = ra
    size[ra] += size[rb]
}

let limit = min(1000, edges.count)
for i in 0..<limit {
    union(edges[i].u, edges[i].v)
}

var circuitSizes = [Int]()
for i in 0..<points.count where parent[i] == i {
    circuitSizes.append(size[i])
}
circuitSizes.sort(by: >)

var result = 1
for i in 0..<min(3, circuitSizes.count) {
    result *= circuitSizes[i]
}
print(result)
