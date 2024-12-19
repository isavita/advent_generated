
import Foundation

func solve() {
    guard let file = try? String(contentsOfFile: "input.txt") else { return }
    let grid = file.components(separatedBy: .newlines).filter { !$0.isEmpty }
    let h = grid.count
    guard let w = grid.first?.count else { return }

    var antennas = [Character: [[Int]]]()
    for y in 0..<h {
        for x in 0..<w {
            let c = grid[y][grid[y].index(grid[y].startIndex, offsetBy: x)]
            if c != "." {
                antennas[c, default: []].append([y, x])
            }
        }
    }

    var antinodes = Set<[Int]>()
    for coords in antennas.values {
        let n = coords.count
        for i in 0..<n {
            for j in i+1..<n {
                let A = coords[i]
                let B = coords[j]
                let P1 = [2*A[0] - B[0], 2*A[1] - B[1]]
                let P2 = [2*B[0] - A[0], 2*B[1] - A[1]]
                if P1[0] >= 0 && P1[0] < h && P1[1] >= 0 && P1[1] < w {
                    antinodes.insert(P1)
                }
                if P2[0] >= 0 && P2[0] < h && P2[1] >= 0 && P2[1] < w {
                    antinodes.insert(P2)
                }
            }
        }
    }
    print(antinodes.count)
}

solve()
