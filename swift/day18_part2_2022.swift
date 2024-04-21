import Foundation

struct Pt3: Hashable {
    let x, y, z: Int

    init(_ x: Int, _ y: Int, _ z: Int) {
        self.x = x
        self.y = y
        self.z = z
    }

    func adding(_ p: Pt3) -> Pt3 {
        return Pt3(x + p.x, y + p.y, z + p.z)
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(x)
        hasher.combine(y)
        hasher.combine(z)
    }
}

func min(_ a: Int, _ b: Int) -> Int {
    return a < b ? a : b
}

func max(_ a: Int, _ b: Int) -> Int {
    return a > b ? a : b
}

var cubes: [Pt3: Void] = [:]
var neighbors: [Pt3] = [
    Pt3(-1, 0, 0),
    Pt3(1, 0, 0),
    Pt3(0, -1, 0),
    Pt3(0, 1, 0),
    Pt3(0, 0, -1),
    Pt3(0, 0, 1)
]

var minPt = Pt3(Int.max, Int.max, Int.max)
var maxPt = Pt3(Int.min, Int.min, Int.min)

do {
    let fileURL = URL(fileURLWithPath: "input.txt")
    let content = try String(contentsOf: fileURL, encoding: .utf8)
    let lines = content.components(separatedBy: "\n")

    for line in lines {
        if line.isEmpty {
            continue
        }
        let components = line.components(separatedBy: ",")
        guard components.count == 3,
              let x = Int(components[0]),
              let y = Int(components[1]),
              let z = Int(components[2]) else {
            fatalError()
        }
        let cube = Pt3(x, y, z)
        cubes[cube] = ()
        minPt = Pt3(min(minPt.x, x), min(minPt.y, y), min(minPt.z, z))
        maxPt = Pt3(max(maxPt.x, x), max(maxPt.y, y), max(maxPt.z, z))
    }
}

minPt = minPt.adding(Pt3(-1, -1, -1))
maxPt = maxPt.adding(Pt3(1, 1, 1))

var faces = 0
var q: [Pt3] = [minPt]
var seen: [Pt3: Void] = [minPt: ()]

while !q.isEmpty {
    let curr = q.removeFirst()
    for delta in neighbors {
        let next = curr.adding(delta)
        if next.x < minPt.x || next.y < minPt.y || next.z < minPt.z ||
            next.x > maxPt.x || next.y > maxPt.y || next.z > maxPt.z {
            continue
        }
        if cubes[next] != nil {
            faces += 1
        } else if seen[next] == nil {
            seen[next] = ()
            q.append(next)
        }
    }
}

print(faces)