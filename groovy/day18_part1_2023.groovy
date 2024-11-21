
class Coord {
    int x, y

    Coord add(Coord other) {
        new Coord(x: x + other.x, y: y + other.y)
    }

    Coord multiplyByScalar(int scalar) {
        new Coord(x: x * scalar, y: y * scalar)
    }

    static final Coord NORTH = new Coord(x: 0, y: -1)
    static final Coord WEST = new Coord(x: -1, y: 0)
    static final Coord SOUTH = new Coord(x: 0, y: 1)
    static final Coord EAST = new Coord(x: 1, y: 0)

    static int abs(int value) {
        Math.abs(value)
    }
}

def parseInput(List<String> input) {
    def current = new Coord(x: 0, y: 0)
    def vertices = [current]

    input.each { line ->
        def (dirInput, lengthStr) = line.split(' ')
        def length = lengthStr.toInteger()
        def dir

        switch (dirInput) {
            case 'U': dir = Coord.NORTH; break
            case 'L': dir = Coord.WEST; break
            case 'D': dir = Coord.SOUTH; break
            case 'R': dir = Coord.EAST; break
        }

        current = current.add(dir.multiplyByScalar(length))
        vertices << current
    }

    vertices
}

def shoelace(List<Coord> vertices) {
    def n = vertices.size()
    def area = 0

    (0..<n).each { i ->
        def next = (i + 1) % n
        area += vertices[i].x * vertices[next].y - vertices[i].y * vertices[next].x
    }

    Coord.abs(area) / 2
}

def perimeter(List<Coord> vertices) {
    def n = vertices.size()
    def perim = 0

    (0..<n).each { i ->
        def next = (i + 1) % n
        perim += Coord.abs(vertices[i].x - vertices[next].x) + Coord.abs(vertices[i].y - vertices[next].y)
    }

    perim
}

def calculatePolygonArea(List<Coord> vertices) {
    shoelace(vertices) + perimeter(vertices) / 2 + 1
}

def solve(List<String> input) {
    def vertices = parseInput(input)
    calculatePolygonArea(vertices)
}

def input = new File('input.txt').readLines()
println solve(input)
