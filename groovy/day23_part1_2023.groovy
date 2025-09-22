class Coord {
    int x
    int y
    Coord(int x, int y) { this.x = x; this.y = y }
}

class Edge {
    Coord start
    Coord end
    int weight
    Edge(Coord s, Coord e, int w) { this.start = s; this.end = e; this.weight = w }
}

class Grid {
    int width
    int height
    char[][] data
    Grid(int w, int h, char[][] d) { this.width = w; this.height = h; this.data = d }
}

class Graph {
    List<Coord> vertices = []
    List<List<Edge>> edges = []
}

class Main {
    static final int[] DX = [0, 0, -1, 1] as int[]
    static final int[] DY = [-1, 1, 0, 0] as int[]

    static boolean isValid(Grid grid, Coord c) {
        c.x >= 0 && c.x < grid.width && c.y >= 0 && c.y < grid.height && grid.data[c.y][c.x] != '#'
    }

    static boolean isValidWithSlopes(Grid grid, Coord coord, Coord dir) {
        if (!isValid(grid, coord)) return false
        char ch = grid.data[coord.y][coord.x]
        if (ch == '^') return dir.y == -1
        if (ch == 'v') return dir.y == 1
        if (ch == '<') return dir.x == -1
        if (ch == '>') return dir.x == 1
        return true
    }

    static boolean coordEq(Coord a, Coord b) { a.x == b.x && a.y == b.y }

    static boolean vertexIn(Graph graph, Coord c) {
        for (Coord v : graph.vertices) if (coordEq(v, c)) return true
        return false
    }

    static int indexOf(Graph graph, Coord c) {
        for (int i = 0; i < graph.vertices.size(); i++) {
            Coord v = graph.vertices.get(i)
            if (coordEq(v, c)) return i
        }
        return -1
    }

    static void getEdgesBFS(Grid grid, Coord start, Graph graph, Closure isValidFunc, int startIdx) {
        int size = grid.width * grid.height
        List<Coord> frontier = new ArrayList<>()
        int[] distances = new int[size]
        boolean[] reached = new boolean[size]

        frontier.add(start)
        reached[start.y * grid.width + start.x] = true
        distances[start.y * grid.width + start.x] = 0

        int front = 0
        while (front < frontier.size()) {
            Coord current = frontier.get(front++)
            int currentIdx = current.y * grid.width + current.x

            if (vertexIn(graph, current) && !(current.x == start.x && current.y == start.y)) {
                if (graph.edges.get(startIdx) == null) graph.edges.set(startIdx, new ArrayList<Edge>())
                graph.edges.get(startIdx).add(new Edge(start, current, distances[currentIdx]))
                continue
            }

            for (int i = 0; i < 4; i++) {
                int nx = current.x + DX[i]
                int ny = current.y + DY[i]
                Coord next = new Coord(nx, ny)
                int nextIdx = ny * grid.width + nx
                Coord dir = new Coord(DX[i], DY[i])
                if (isValidFunc.call(grid, next, dir) && !reached[nextIdx]) {
                    frontier.add(next)
                    reached[nextIdx] = true
                    distances[nextIdx] = distances[currentIdx] + 1
                }
            }
        }
    }

    static Graph getGraph(Grid grid, Coord start, Coord end, Closure isValidFunc) {
        Graph graph = new Graph()
        graph.vertices.add(start)
        graph.vertices.add(end)

        for (int y = 0; y < grid.height; y++) {
            for (int x = 0; x < grid.width; x++) {
                if (grid.data[y][x] == '.') {
                    Coord coord = new Coord(x, y)
                    int validNeighbors = 0
                    for (int i = 0; i < 4; i++) {
                        Coord nb = new Coord(coord.x + DX[i], coord.y + DY[i])
                        if (isValid(grid, nb)) validNeighbors++
                    }
                    if (validNeighbors > 2) graph.vertices.add(coord)
                }
            }
        }

        graph.edges = new ArrayList<List<Edge>>()
        for (int i = 0; i < graph.vertices.size(); i++) graph.edges.add(null)

        for (int i = 0; i < graph.vertices.size(); i++) {
            getEdgesBFS(grid, graph.vertices.get(i), graph, isValidFunc, i)
        }

        return graph
    }

    static int indexOfVertex(Graph graph, Coord c) {
        return indexOf(graph, c)
    }

    static int maxDistanceDFS(Graph graph, int current, int end, boolean[] seen) {
        if (current == end) return 0

        int maxi = -1
        seen[current] = true

        List<Edge> edges = graph.edges.get(current)
        if (edges != null) {
            for (Edge e : edges) {
                int next = indexOfVertex(graph, e.end)
                if (next >= 0 && !seen[next]) {
                    int dist = maxDistanceDFS(graph, next, end, seen)
                    if (dist >= 0) {
                        int cand = dist + e.weight
                        if (cand > maxi) maxi = cand
                    }
                }
            }
        }

        seen[current] = false
        return maxi >= 0 ? maxi : -1
    }

    static int solve(List<String> lines) {
        int height = lines.size()
        int width = lines.get(0).length()
        char[][] data = new char[height][]
        for (int i = 0; i < height; i++) data[i] = lines.get(i).toCharArray()

        Grid grid = new Grid(width, height, data)
        Coord start = new Coord(1, 0)
        Coord end = new Coord(width - 2, height - 1)

        Closure isValidClosure = { Grid g, Coord c, Coord dir -> isValidWithSlopes(g, c, dir) }

        Graph graph = getGraph(grid, start, end, isValidClosure)

        int maxDist = -1
        if (graph.vertices.size() >= 2) {
            boolean[] seen = new boolean[graph.vertices.size()]
            maxDist = maxDistanceDFS(graph, 0, 1, seen)
        }
        return maxDist
    }

    static int max(int a, int b) { return a > b ? a : b }

    static void main(String[] args) {
        def lines = new File("input.txt").readLines()
        def result = solve(lines)
        println(result)
    }
}

Main.main(null)