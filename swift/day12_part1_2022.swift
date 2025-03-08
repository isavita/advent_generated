
import Foundation

struct Point: Hashable {
    let x: Int
    let y: Int
}

func djikstra(grid: [Point: Character], end: Point) -> [Point: Int] {
    var pq = [(0, end)]
    var dist: [Point: Int] = [end: 0]
    
    while !pq.isEmpty {
        pq.sort { $0.0 < $1.0 }
        let (d, curr) = pq.removeFirst()
        
        if d > dist[curr]! {
            continue
        }

        let neighbors = [(0, 1), (0, -1), (1, 0), (-1, 0)]
        for n in neighbors {
            let next = Point(x: curr.x + n.0, y: curr.y + n.1)
            guard let currChar = grid[curr], let nextChar = grid[next] else {
                continue
            }
            if Int(currChar.asciiValue!) - Int(nextChar.asciiValue!) > 1 {
                continue
            }
            let nextDist = dist[curr]! + 1
            if dist[next] == nil || nextDist < dist[next]! {
                dist[next] = nextDist
                pq.append((nextDist, next))
            }
        }
    }
    return dist
}

func main() {
    guard let s = try? String(contentsOfFile: "input.txt") else {
        return
    }

    var grid: [Point: Character] = [:]
    var start: Point?
    var end: Point?
    var as_points: [Point] = []
    var y = 0

    for line in s.split(separator: "\n") {
        for (x, b) in line.enumerated() {
            let p = Point(x: x, y: y)
            grid[p] = b
            if b == "S" {
                start = p
            } else if b == "E" {
                end = p
            } else if b == "a"{
                as_points.append(p)
            }
        }
        y += 1
    }
   
    grid[start!] = "a"
    grid[end!] = "z"

    let dists = djikstra(grid: grid, end: end!)
    print(dists[start!]!)
}

main()
