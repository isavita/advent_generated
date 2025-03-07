
import Foundation

struct State: Comparable {
    let cost: Int
    let x: Int
    let y: Int
    let dir: Int

    static func < (lhs: State, rhs: State) -> Bool {
        lhs.cost < rhs.cost
    }
}

func solve() {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        fatalError("Could not read input.txt")
    }
    let grid = input.split(separator: "\n").map { String($0) }
    let n = grid.count
    let m = grid[0].count

    var sx = 0, sy = 0, ex = 0, ey = 0
    for r in 0..<n {
        for c in 0..<m where grid[r][String.Index(utf16Offset: c, in: grid[r])] == "S" {
            (sx, sy) = (r, c)
        }
        for c in 0..<m where grid[r][String.Index(utf16Offset: c, in: grid[r])] == "E" {
            (ex, ey) = (r, c)
        }
    }

    let dx = [-1, 0, 1, 0]
    let dy = [0, 1, 0, -1]

    var dist = Array(repeating: Array(repeating: Array(repeating: Int.max, count: 4), count: m), count: n)
    dist[sx][sy][1] = 0

    var pq = [State(cost: 0, x: sx, y: sy, dir: 1)]

    while !pq.isEmpty {
        let state = pq.min()!
        pq.removeAll(where: { $0 == state })

        let cost = state.cost
        let x = state.x
        let y = state.y
        let d = state.dir

        if cost > dist[x][y][d] {
            continue
        }
        
        for ndir in [(d + 1) % 4, (d + 3) % 4] {
            let nc = cost + 1000
            if nc < dist[x][y][ndir] {
                dist[x][y][ndir] = nc
                pq.append(State(cost: nc, x: x, y: y, dir: ndir))
            }
        }

        let nx = x + dx[d]
        let ny = y + dy[d]

        if 0 <= nx && nx < n && 0 <= ny && ny < m && grid[nx][String.Index(utf16Offset: ny, in: grid[nx])] != "#" {
            let nc = cost + 1
            if nc < dist[nx][ny][d] {
                dist[nx][ny][d] = nc
                pq.append(State(cost: nc, x: nx, y: ny, dir: d))
            }
        }
    }

    let best = dist[ex][ey].min()!

    var used = Array(repeating: Array(repeating: false, count: m), count: n)
    var rev = (0..<4).filter { dist[ex][ey][$0] == best }.map { (ex, ey, $0) }
    var vis = Array(repeating: Array(repeating: Array(repeating: false, count: 4), count: m), count: n)

    for (x, y, d) in rev {
        vis[x][y][d] = true
    }
    
    var revIndex = 0
    while revIndex < rev.count {
      let (x, y, d) = rev[revIndex]
      revIndex += 1
        used[x][y] = true
        let costU = dist[x][y][d]

        for pd in [(d + 1) % 4, (d + 3) % 4] {
            if dist[x][y][pd] == costU - 1000 && !vis[x][y][pd] {
                vis[x][y][pd] = true
                rev.append((x, y, pd))
            }
        }

        let px = x - dx[d]
        let py = y - dy[d]
        if 0 <= px && px < n && 0 <= py && py < m && grid[px][String.Index(utf16Offset: py, in: grid[px])] != "#" && dist[px][py][d] == costU - 1 && !vis[px][py][d] {
            vis[px][py][d] = true
            rev.append((px, py, d))
        }
    }

    let count = used.reduce(0) { $0 + $1.filter { $0 }.count }
    print(count)
}

solve()
