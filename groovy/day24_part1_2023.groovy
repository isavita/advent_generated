
class Coord {
    def x
    def y
    def z
}

class Point {
    Coord pos
    Coord vel
}

def parseInput(input) {
    def points = []
    input.eachWithIndex { line, i ->
        def point = new Point()
        def values = line.tokenize(", @ ")
        point.pos = new Coord(x: values[0] as Float, y: values[1] as Float, z: values[2] as Float)
        point.vel = new Coord(x: values[3] as Float, y: values[4] as Float, z: values[5] as Float)
        points.add(point)
    }
    return points
}

def isIntersecting2D(p1, p2) {
    def det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y
    if (det == 0) {
        return [false, new Coord(), 0, 0]
    }
    def t1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det
    def t2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det
    def coord = new Coord(x: p1.pos.x + p1.vel.x * t1, y: p1.pos.y + p1.vel.y * t1, z: 0)
    return [true, coord, t1, t2]
}

def solve(input, min, max) {
    def points = parseInput(input)

    def cnt = 0
    points.eachWithIndex { p1, i ->
        (0..<i).each { j ->
            def p2 = points[j]
            def (isIntersecting, coord, time1, time2) = isIntersecting2D(p1, p2)
            def isInBound = min <= coord.x && coord.x <= max && min <= coord.y && coord.y <= max
            if (isIntersecting && isInBound && time1 >= 0 && time2 >= 0) {
                cnt++
            }
        }
    }
    return cnt
}

def readFile(fileName) {
    new File(fileName).text.tokenize("\n")
}

def input = readFile("input.txt")
println solve(input, 200000000000000, 400000000000000)
