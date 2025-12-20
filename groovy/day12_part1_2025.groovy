
import java.nio.file.*

class Point implements Comparable<Point> {
    int x, y
    int compareTo(Point o) { y != o.y ? y <=> o.y : x <=> o.x }
    boolean equals(Object o) { o instanceof Point && x == o.x && y == o.y }
    int hashCode() { Objects.hash(x, y) }
}

class Orientation {
    Point[] points
    int w, h
}

class Shape {
    List<Orientation> orientations
    int area
    Shape(List<String> rows) {
        def pts = [] as List<Point>
        rows.eachWithIndex { row, r -> row.eachWithIndex { ch, c -> if (ch == '#') pts << new Point(x: c, y: r) } }
        area = pts.size()
        orientations = generateAllOrientations(pts)
    }
    private List<Orientation> generateAllOrientations(List<Point> points) {
        def uniq = new HashSet<List<Point>>()
        (0..7).each { i ->
            def trans = points.collect { p ->
                int nx = 0, ny = 0
                switch (i) {
                    case 0: nx = p.x; ny = p.y; break
                    case 1: nx = p.y; ny = -p.x; break
                    case 2: nx = -p.x; ny = -p.y; break
                    case 3: nx = -p.y; ny = p.x; break
                    case 4: nx = -p.x; ny = p.y; break
                    case 5: nx = p.y; ny = p.x; break
                    case 6: nx = p.x; ny = -p.y; break
                    case 7: nx = -p.y; ny = -p.x; break
                }
                new Point(x: nx, y: ny)
            }
            def minX = trans*.x.min()
            def minY = trans*.y.min()
            trans.each { it.x -= minX; it.y -= minY }
            Collections.sort(trans)
            uniq << trans
        }
        uniq.collect { pts ->
            def o = new Orientation()
            o.points = pts as Point[]
            o.w = (pts*.x.max() ?: -1) + 1
            o.h = (pts*.y.max() ?: -1) + 1
            o
        }
    }
}

class Solver {
    static int totalPossible = 0
    static void handleRegion(String line, List<Shape> allShapes) {
        def (dimPart, cntPart) = line.split(':')
        def (W, H) = dimPart.trim().split('x')*.toInteger()
        def counts = cntPart.trim().split(/\s+/)*.toInteger()
        def pToFit = [] as List<Shape>
        int totalArea = 0
        counts.eachWithIndex { qty, i ->
            if (i >= allShapes.size()) return
            qty.times {
                pToFit << allShapes[i]
                totalArea += allShapes[i].area
            }
        }
        pToFit.sort { -it.area }
        if (solve(0, new boolean[W * H], W, H, pToFit, totalArea, W * H)) totalPossible++
    }
    static boolean solve(int idx, boolean[] grid, int W, int H, List<Shape> shapes, int remArea, int freeArea) {
        if (idx == shapes.size()) return true
        if (remArea > freeArea) return false
        def shape = shapes[idx]
        for (orient in shape.orientations) {
            if (orient.w > W || orient.h > H) continue
            for (r in 0..(H - orient.h)) {
                for (c in 0..(W - orient.w)) {
                    boolean ok = true
                    for (p in orient.points) {
                        if (grid[(r + p.y) * W + (c + p.x)]) { ok = false; break }
                    }
                    if (!ok) continue
                    for (p in orient.points) grid[(r + p.y) * W + (c + p.x)] = true
                    if (solve(idx + 1, grid, W, H, shapes, remArea - shape.area, freeArea - shape.area)) return true
                    for (p in orient.points) grid[(r + p.y) * W + (c + p.x)] = false
                }
            }
        }
        false
    }
}

def lines = Files.readAllLines(Paths.get('input.txt'))
def allShapes = [] as List<Shape>
def it = lines.iterator()
while (it.hasNext()) {
    def line = it.next().trim()
    if (!line) continue
    if (line.contains('x') && line.contains(':')) {
        Solver.handleRegion(line, allShapes)
    } else if (line.endsWith(':')) {
        def rows = [] as List<String>
        while (it.hasNext()) {
            def nxt = it.next().trim()
            if (!nxt || nxt.contains(':')) break
            if (nxt.contains('#') || nxt.contains('.')) rows << nxt
            else break
        }
        if (rows) allShapes << new Shape(rows)
    }
}
println Solver.totalPossible
