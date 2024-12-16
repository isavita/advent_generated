
import java.awt.Point

def solve() {
    def lines = new File("input.txt").readLines()
    
    def instructions = lines.collect { line ->
        def parts = line.split(" ")
        def hexCode = parts[2].substring(2, 8)
        def distance = Integer.parseInt(hexCode.substring(0, 5), 16)
        def direction = hexCode[-1] as int
        
        def dirChar = direction == 0 ? 'R' : direction == 1 ? 'D' : direction == 2 ? 'L' : 'U'
        
        return [dirChar, distance]
    }

    def points = []
    def current = new Point(0, 0)
    points << current.clone()

    long boundaryPoints = 0
    
    instructions.each { instruction ->
        def dir = instruction[0]
        def dist = instruction[1] as int
        
        boundaryPoints += dist
        
        switch (dir) {
            case 'R': current.x += dist; break
            case 'L': current.x -= dist; break
            case 'U': current.y -= dist; break
            case 'D': current.y += dist; break
        }
        points << current.clone()
    }

    long area = 0
    for (int i = 0; i < points.size() - 1; i++) {
        def p1 = points[i]
        def p2 = points[i + 1]
        area += (long)p1.x * p2.y - (long)p2.x * p1.y
    }
    area = Math.abs(area) / 2

    long interiorPoints = area - boundaryPoints / 2 + 1
    long totalPoints = interiorPoints + boundaryPoints
    
    println totalPoints
}

solve()
