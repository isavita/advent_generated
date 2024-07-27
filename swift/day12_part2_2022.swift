
import Foundation

struct Point: Hashable {
    let x: Int
    let y: Int
}

func main() {
    var grid = [Point: Character]()
    var start: Point?
    var end: Point?
    var asPoints = [Point]()
    var y = 0

    let input = try! String(contentsOfFile: "input.txt")
    for line in input.split(separator: "\n") {
        for (x, char) in line.enumerated() {
            let p = Point(x: x, y: y)
            grid[p] = char
            if char == "S" { start = p }
            else if char == "E" { end = p }
            else if char == "a" { asPoints.append(p) }
        }
        y += 1
    }
    
    if let start = start, let end = end {
        grid[start] = "a"
        grid[end] = "z"
        let dists = dijkstra(grid: grid, end: end)
        var l = dists[start] ?? Int.max
        
        for a in asPoints {
            if let d = dists[a] {
                l = min(l, d)
            }
        }
        print(l)
    }
}

func dijkstra(grid: [Point: Character], end: Point) -> [Point: Int] {
    var pq = [(point: Point, distance: Int)]()
    var dist = [Point: Int]()
    pq.append((end, 0))
    dist[end] = 0
    
    let neighbors = [Point(x: 0, y: 1), Point(x: 0, y: -1), Point(x: 1, y: 0), Point(x: -1, y: 0)]
    
    while !pq.isEmpty {
        pq.sort { $0.distance < $1.distance }
        let (curr, currDist) = pq.removeFirst()
        
        for n in neighbors {
            let next = Point(x: curr.x + n.x, y: curr.y + n.y)
            guard let nextChar = grid[next] else { continue }
            if Int(grid[curr]!.asciiValue!) - Int(nextChar.asciiValue!) > 1 { continue }
            let nextDist = currDist + 1
            
            if dist[next] == nil || nextDist < dist[next]! {
                dist[next] = nextDist
                pq.append((next, nextDist))
            }
        }
    }
    return dist
}

main()
