
class Coord {
    int x, y

    Coord(int x, int y) {
        this.x = x
        this.y = y
    }

    Coord plus(Coord other) {
        new Coord(x + other.x, y + other.y)
    }

    @Override
    boolean equals(Object o) {
        if (this.is(o)) return true
        if (getClass() != o.class) return false
        Coord coord = (Coord) o
        return x == coord.x && y == coord.y
    }

    @Override
    int hashCode() {
        return Objects.hash(x, y)
    }

    @Override
    String toString() {
        return "Coord{" +
                "x=" + x +
                ", y=" + y +
                '}';
    }
}

class Grid {
    int width, height
    Map<Coord, Character> data = new HashMap<>()

    Grid(int width, int height) {
        this.width = width
        this.height = height
    }
}

class Edge {
    Coord start, end
    int weight

    Edge(Coord start, Coord end, int weight) {
        this.start = start
        this.end = end
        this.weight = weight
    }

    @Override
    String toString() {
        return "Edge{" +
                "start=" + start +
                ", end=" + end +
                ", weight=" + weight +
                '}';
    }
}

class Graph {
    Set<Coord> vertices = new HashSet<>()
    Map<Coord, List<Edge>> edges = new HashMap<>()
}

enum Direction {
    North(0, -1),
    South(0, 1),
    West(-1, 0),
    East(1, 0);

    final int dx, dy

    Direction(int dx, int dy) {
        this.dx = dx
        this.dy = dy
    }

    Coord toCoord() {
        new Coord(dx, dy)
    }
}

class Solution {

    static final char EMPTY = '.'
    static final char WALL = '#'
    static final char NORTH_SLOPES = '^'
    static final char SOUTH_SLOPES = 'v'
    static final char WEST_SLOPES = '<'
    static final char EAST_SLOPES = '>'

    static final Map<Character, Direction> SLOPE_TO_DIR = [
            (NORTH_SLOPES): Direction.North,
            (SOUTH_SLOPES): Direction.South,
            (WEST_SLOPES): Direction.West,
            (EAST_SLOPES): Direction.East
    ]

    static boolean isInBounds(Grid grid, Coord coord) {
        return 0 <= coord.x && coord.x < grid.width && 0 <= coord.y && coord.y < grid.height
    }

    static Grid parseInput(List<String> input) {
        Grid grid = new Grid(input[0].length(), input.size())
        for (int y = 0; y < input.size(); y++) {
            String line = input[y]
            for (int x = 0; x < line.length(); x++) {
                char c = line[x]
                if (c != EMPTY) {
                    grid.data.put(new Coord(x, y), c)
                }
            }
        }
        return grid
    }

    static List<Coord> neighbors4(Grid grid, Coord coord, boolean allowSlopes) {
        List<Coord> validNeighbors = []
        Direction.values().each { dir ->
            Coord neighbor = coord.plus(dir.toCoord())
            if (isInBounds(grid, neighbor)) {
                char cell = grid.data.getOrDefault(neighbor, EMPTY)
                if (cell != WALL) {
                    if (allowSlopes || cell == EMPTY || SLOPE_TO_DIR.get(cell) == dir)
                    validNeighbors << neighbor
                }
            }
        }
        return validNeighbors
    }

    static Graph getGraph(Grid grid, Coord start, Coord end, boolean allowSlopes) {
        Graph graph = new Graph()
        graph.vertices.add(start)
        graph.vertices.add(end)

        for (int y = 0; y < grid.height; y++) {
            for (int x = 0; x < grid.width; x++) {
                Coord coord = new Coord(x, y)
                if (!grid.data.containsKey(coord) || grid.data.get(coord) == EMPTY) {
                    if (neighbors4(grid, coord, allowSlopes).size() > 2) {
                        graph.vertices.add(coord)
                    }
                } else if (grid.data.get(coord) != WALL && SLOPE_TO_DIR.containsKey(grid.data.get(coord)) && allowSlopes) {
                     graph.vertices.add(coord)
                }
            }
        }

        graph.vertices.each { startNode ->
            List<Edge> edges = getEdgesBfs(grid, startNode, graph.vertices, allowSlopes)
            graph.edges.put(startNode, edges)
        }

        return graph
    }

    static List<Edge> getEdgesBfs(Grid grid, Coord start, Set<Coord> vertices, boolean allowSlopes) {
        Queue<Coord> frontier = new LinkedList<>()
        frontier.add(start)
        Set<Coord> reached = new HashSet<>()
        reached.add(start)
        Map<Coord, Integer> distances = [(start): 0]
        List<Edge> edges = []

        while (!frontier.isEmpty()) {
            Coord current = frontier.remove()

            if (vertices.contains(current) && current != start) {
                Edge edge = new Edge(start, current, distances.get(current))
                edges.add(edge)
                continue
            }

            neighbors4(grid, current, allowSlopes).each { nextNode ->
                if (!reached.contains(nextNode)) {
                    frontier.add(nextNode)
                    reached.add(nextNode)
                    distances.put(nextNode, distances.get(current) + 1)
                }
            }
        }

        return edges
    }

    static int getMaxDistanceDfs(Graph graph, Coord current, Coord end, Set<Coord> seen) {
        if (current == end) {
            return 0
        }

        seen.add(current)
        int maxDist = -1

        List<Edge> currentEdges = graph.edges.get(current)
        if (currentEdges != null) {
            currentEdges.each { edge ->
                if (!seen.contains(edge.end)) {
                    int dist = getMaxDistanceDfs(graph, edge.end, end, seen)
                    if (dist != -1) {
                        maxDist = Math.max(maxDist, dist + edge.weight)
                    }
                }
            }
        }

        seen.remove(current)
        return maxDist
    }

    static int solve(List<String> input, boolean allowSlopes) {
        Grid grid = parseInput(input)
        Coord start = new Coord(1, 0)
        Coord end = new Coord(grid.width - 2, grid.height - 1)

        Graph graph = getGraph(grid, start, end, allowSlopes)

        return getMaxDistanceDfs(graph, start, end, new HashSet<>())
    }
}

def main() {
    File file = new File("input.txt")
    List<String> input = file.readLines()
    println Solution.solve(input, false) // Part 1
    println Solution.solve(input, true)  // Part 2
}

main()
