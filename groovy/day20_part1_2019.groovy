
class Day20 {
    static final char WALL = '#' as char
    static final char FREE = '.' as char

    static class Point {
        int x, y
        
        Point(int x, int y) {
            this.x = x
            this.y = y
        }
        
        List<Point> getNeighbors() {
            [
                new Point(x, y+1),
                new Point(x+1, y),
                new Point(x, y-1),
                new Point(x-1, y)
            ]
        }
        
        boolean equals(Object o) {
            if (!(o instanceof Point)) return false
            Point p = (Point)o
            return x == p.x && y == p.y
        }
        
        int hashCode() {
            Objects.hash(x, y)
        }
    }

    static void main(String[] args) {
        def m = parse()
        println bfs(m)
    }

    static Map parse() {
        def grid = [:] as Map<Point, Character>
        def xMax = 0
        def yMax = 0

        def lines = new File('input.txt').readLines()
        lines.eachWithIndex { line, i ->
            yMax = Math.max(yMax, line.length())
            line.toCharArray().eachWithIndex { c, j ->
                grid[new Point(i, j)] = c
            }
            xMax = i
        }

        def aa = null
        def zz = null
        def isOuter = [:] as Map<Point, Boolean>
        def portalName = [:] as Map<Point, String>
        def teleport = [:] as Map<Point, Point>
        def cache = [:] as Map<String, Point>

        for (int i = 0; i < xMax; i++) {
            for (int j = 0; j < yMax; j++) {
                def p = new Point(i, j)
                def c = grid[p]

                if (!isLetter(c)) continue

                def (pName, pPoint, ok) = extractPortal(grid, p)

                if (!ok) continue

                portalName[pPoint] = pName

                if (pName == "AA") {
                    aa = pPoint
                    isOuter[pPoint] = true
                    continue
                }

                if (pName == "ZZ") {
                    zz = pPoint
                    isOuter[pPoint] = true
                    continue
                }

                if (cache.containsKey(pName)) {
                    def target = cache[pName]
                    teleport[pPoint] = target
                    teleport[target] = pPoint
                } else {
                    cache[pName] = pPoint
                }

                isOuter[pPoint] = (j == 0 || i == 0 || i == xMax-2 || j == yMax-2)
            }
        }

        [
            xMax: xMax,
            yMax: yMax,
            grid: grid,
            AA: aa,
            ZZ: zz,
            teleport: teleport,
            portalName: portalName,
            isOuter: isOuter
        ]
    }

    static List extractPortal(Map grid, Point p) {
        def c1 = grid[p]

        // Horizontal portal
        def c2 = grid[new Point(p.x + 1, p.y)]
        if (isLetter(c2)) {
            def portalName = c1.toString() + c2.toString()
            def portalPoint = new Point(p.x + 2, p.y)
            if (grid[portalPoint] == FREE) {
                return [portalName, portalPoint, true]
            }
            portalPoint = new Point(p.x - 1, p.y)
            if (grid[portalPoint] == FREE) {
                return [portalName, portalPoint, true]
            }
        }

        // Vertical portal
        c2 = grid[new Point(p.x, p.y + 1)]
        if (isLetter(c2)) {
            def portalName = c1.toString() + c2.toString()
            def portalPoint = new Point(p.x, p.y + 2)
            if (grid[portalPoint] == FREE) {
                return [portalName, portalPoint, true]
            }
            portalPoint = new Point(p.x, p.y - 1)
            if (grid[portalPoint] == FREE) {
                return [portalName, portalPoint, true]
            }
        }

        [null, null, false]
    }

    static boolean isLetter(Character c) {
        c >= 'A' && c <= 'Z'
    }

    static int bfs(Map m) {
        def discovered = new HashSet<Point>()
        def toDo = new LinkedList<Point>()

        discovered.add(m.AA)
        toDo.add(m.AA)

        int depth = 0

        while (!toDo.isEmpty()) {
            def levelSize = toDo.size()
            for (int i = 0; i < levelSize; i++) {
                def curr = toDo.poll()

                if (curr == m.ZZ) {
                    return depth
                }

                for (def n : curr.neighbors) {
                    def dest = m.grid[n]

                    switch (dest) {
                        case WALL:
                            continue
                        case FREE:
                            if (!discovered.contains(n)) {
                                discovered.add(n)
                                toDo.add(n)
                            }
                            break
                        default:
                            if (isLetter(dest)) {
                                def next = m.teleport[curr]
                                if (next && !discovered.contains(next)) {
                                    discovered.add(next)
                                    toDo.add(next)
                                }
                            }
                            break
                    }
                }
            }
            depth++
        }

        -1
    }
}
