
import std.stdio;
import std.file;
import std.string;
import std.algorithm;
import std.container.dlist;

struct Coord {
    int x, y;
    Coord opBinary(string op)(Coord rhs) if (op == "+") {
        return Coord(x + rhs.x, y + rhs.y);
    }
}

struct Grid {
    int width, height;
    char[Coord] data;
}

struct Edge {
    int weight;
}

struct Graph {
    bool[Coord] vertices;
    Edge[Coord][Coord] edges;
}

Grid parseInput(string[] lines) {
    Grid grid;
    grid.height = cast(int)lines.length;
    if (grid.height == 0) return grid;
    grid.width = cast(int)lines[0].length;

    foreach (y, line; lines) {
        foreach (x, c; line) {
            if (c != '.') {
                grid.data[Coord(cast(int)x, cast(int)y)] = c;
            }
        }
    }
    return grid;
}

bool isInBounds(const ref Grid grid, Coord coord) {
    return 0 <= coord.x && coord.x < grid.width && 0 <= coord.y && coord.y < grid.height;
}

bool isValidNeighbor(const ref Grid grid, Coord coord) {
    if (!isInBounds(grid, coord)) return false;
    auto p = coord in grid.data;
    return !(p && *p == '#');
}

Coord[] neighbors4(const ref Grid grid, Coord coord) {
    immutable Coord[4] directions = [Coord(0, -1), Coord(0, 1), Coord(-1, 0), Coord(1, 0)];
    Coord[] validNeighbors;
    foreach (dir; directions) {
        auto neighbor = coord + dir;
        if (isValidNeighbor(grid, neighbor)) {
            validNeighbors ~= neighbor;
        }
    }
    return validNeighbors;
}

Edge[Coord] getEdgesBFS(const ref Grid grid, Coord start, const ref bool[Coord] vertices) {
    auto frontier = DList!Coord(start);
    bool[Coord] reached = [start: true];
    int[Coord] distances = [start: 0];
    Edge[Coord] edges;

    while (!frontier.empty) {
        auto current = frontier.front;
        frontier.removeFront();

        if (current in vertices && current != start) {
            edges[current] = Edge(distances[current]);
            continue;
        }

        foreach (next; neighbors4(grid, current)) {
            if (next !in reached) {
                reached[next] = true;
                frontier.insertBack(next);
                distances[next] = distances[current] + 1;
            }
        }
    }
    return edges;
}

Graph getGraph(const ref Grid grid, Coord start, Coord end) {
    Graph graph;
    graph.vertices[start] = true;
    graph.vertices[end] = true;

    foreach (y; 0 .. grid.height) {
        foreach (x; 0 .. grid.width) {
            auto coord = Coord(x, y);
            if (coord !in grid.data) {
                if (neighbors4(grid, coord).length > 2) {
                    graph.vertices[coord] = true;
                }
            }
        }
    }

    foreach (vertex; graph.vertices.byKey) {
        graph.edges[vertex] = getEdgesBFS(grid, vertex, graph.vertices);
    }

    return graph;
}

int findLongestPath(const ref Graph graph, Coord current, Coord end, ref bool[Coord] seen) {
    if (current == end) return 0;

    seen[current] = true;
    scope(exit) seen.remove(current);

    int maxDist = -1;

    auto currentEdges = current in graph.edges;
    if (currentEdges) {
        foreach (endNode, edge; *currentEdges) {
            if (endNode !in seen) {
                int dist = findLongestPath(graph, endNode, end, seen);
                if (dist != -1) {
                    maxDist = max(maxDist, dist + edge.weight);
                }
            }
        }
    }
    return maxDist;
}

long solve(string[] input) {
    auto grid = parseInput(input);
    auto start = Coord(1, 0);
    auto end = Coord(grid.width - 2, grid.height - 1);

    auto graph = getGraph(grid, start, end);
    
    bool[Coord] seen;
    return findLongestPath(graph, start, end, seen);
}

void main() {
    auto input = readText("input.txt").strip.splitLines;
    solve(input).writeln;
}
