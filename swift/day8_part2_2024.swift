
import Foundation

func gcd(_ a: Int, _ b: Int) -> Int {
    if b == 0 {
        return a < 0 ? -a : a
    }
    return gcd(b, a % b)
}

let fileURL = URL(fileURLWithPath: "input.txt")
let grid = try String(contentsOf: fileURL).split(separator: "\n").map { String($0) }

let h = grid.count
let w = grid[0].count
var antennas: [Character: [[Int]]] = [:]
for y in 0..<h {
    for x in 0..<w {
        let c = grid[y][grid[y].index(grid[y].startIndex, offsetBy: x)]
        if c != "." {
            antennas[c, default: []].append([y, x])
        }
    }
}

var linesPerFreq: [Character: Set<String>] = [:]
for (f, coords) in antennas {
    linesPerFreq[f] = Set<String>()
    let n = coords.count
    for i in 0..<n {
        for j in i + 1..<n {
            let a = coords[i]
            let b = coords[j]
            var dy = b[0] - a[0]
            var dx = b[1] - a[1]
            let g = gcd(dy, dx)
            var sy = dy / g
            var sx = dx / g
            if sx < 0 || (sx == 0 && sy < 0) {
                sx = -sx
                sy = -sy
            }
            let c = sy * a[1] - sx * a[0]
            linesPerFreq[f]!.insert("\(sx),\(sy),\(c)")
        }
    }
}

var antinodes = Set<[Int]>()
for lines in linesPerFreq.values {
    for key in lines {
        let parts = key.split(separator: ",")
        let sx = Int(parts[0])!
        let sy = Int(parts[1])!
        let c = Int(parts[2])!
        if sx == 0 && sy == 0 {
            continue
        }
        if sy == 0 {
            if c % sx == 0 {
                let y = -c / sx
                if y >= 0 && y < h {
                    for x in 0..<w {
                        antinodes.insert([y, x])
                    }
                }
            }
        } else if sx == 0 {
            if c % sy == 0 {
                let x = c / sy
                if x >= 0 && x < w {
                    for y in 0..<h {
                        antinodes.insert([y, x])
                    }
                }
            }
        } else {
            for y in 0..<h {
                let val = c + sx * y
                if val % sy == 0 {
                    let x = val / sy
                    if x >= 0 && x < w {
                        antinodes.insert([y, x])
                    }
                }
            }
        }
    }
}

print(antinodes.count)
