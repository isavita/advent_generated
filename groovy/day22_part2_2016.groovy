
import java.nio.file.Files
import java.nio.file.Paths
import java.util.LinkedList
import java.util.Queue

// Node class to hold parsed data
@groovy.transform.Canonical
class Node {
    int x
    int y
    int size
    int used
    int avail
    int usePercent
}

// Simple Point class for BFS visited map keys and positions
@groovy.transform.Canonical
class Point {
    int x
    int y

    String toString() { "$x,$y" } // For map keys
    boolean equals(other) { other instanceof Point && x == other.x && y == other.y }
    int hashCode() { 31 * x + y } // Consistent with equals
}

// BFS implementation to find shortest path for the empty node
int bfs(Point start, Point target, boolean[][] walls, int maxX, int maxY) {
    def queue = new LinkedList<Point>()
    def visited = new HashMap<String, Integer>() // Map key: "x,y", value: distance

    queue.add(start)
    visited.put(start.toString(), 0)

    def dx = [0, 0, 1, -1] // N, S, E, W movements
    def dy = [-1, 1, 0, 0]

    while (!queue.isEmpty()) {
        def current = queue.poll()
        int dist = visited.get(current.toString())

        // If we reached the target, return the distance
        if (current.x == target.x && current.y == target.y) {
            return dist
        }

        // Explore neighbors
        for (int i = 0; i < 4; i++) {
            int nx = current.x + dx[i]
            int ny = current.y + dy[i]
            def neighbor = new Point(nx, ny)
            def neighborKey = neighbor.toString()

            // Check bounds
            if (nx < 0 || nx > maxX || ny < 0 || ny > maxY) continue

            // Check walls: A node is a wall for the empty slot if the empty slot cannot move into it.
            // The empty slot can move from A to B if B.Used <= A.Avail.
            // If A is the empty slot, this is B.Used <= emptyNode.Avail (which is emptyNode.Size).
            // So, the empty slot cannot move to B if B.Used > emptyNode.Size. This makes B a wall.
            if (walls[ny][nx]) continue

            // Check if visited
            if (!visited.containsKey(neighborKey)) {
                visited.put(neighborKey, dist + 1)
                queue.add(neighbor)
            }
        }
    }

    // Should not happen for this specific puzzle if target is reachable
    return -1
}


static void main(String[] args) {
    def inputFile = new File("input.txt")
    def lines = inputFile.readLines()

    def nodes = []
    def maxX = 0
    def maxY = 0

    def nodeRegex = ~/^\/dev\/grid\/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%$/

    // Skip header lines (usually 2 lines starting with '#' or 'Filesystem')
    lines = lines.dropWhile { it.startsWith('#') || it.startsWith('Filesystem') }

    lines.each { line ->
        def matcher = line =~ nodeRegex
        if (matcher.matches()) {
            def (fullMatch, xStr, yStr, sizeStr, usedStr, availStr, usePercentStr) = matcher[0]
            def node = new Node(
                x: xStr.toInteger(),
                y: yStr.toInteger(),
                size: sizeStr.toInteger(),
                used: usedStr.toInteger(),
                avail: availStr.toInteger(),
                usePercent: usePercentStr.toInteger()
            )
            nodes << node
            maxX = Math.max(maxX, node.x)
            maxY = Math.max(maxY, node.y)
        }
    }

    // --- Part One ---
    // Count viable pairs (A, B) where A is not empty, A!=B, and A.used <= B.avail
    def viablePairs = 0
    for (int i = 0; i < nodes.size(); i++) {
        for (int j = 0; j < nodes.size(); j++) {
            def nodeA = nodes[i]
            def nodeB = nodes[j]

            // Check conditions:
            // 1. A is not empty (its Used is not zero).
            // 2. Nodes A and B are not the same node.
            // 3. The data on node A (its Used) would fit on node B (its Avail).
            if (nodeA.used > 0 && !(nodeA.x == nodeB.x && nodeA.y == nodeB.y) && nodeA.used <= nodeB.avail) {
                viablePairs++
            }
        }
    }
    println "Part One: ${viablePairs}"


    // --- Part Two ---
    // Find the minimum steps to move data from (maxX, 0) to (0, 0).
    // This involves moving the empty node strategically.

    def grid = new Node[maxY + 1][maxX + 1]
    def emptyNode = null

    nodes.each { node ->
        grid[node.y][node.x] = node
        if (node.used == 0) {
            emptyNode = node
        }
    }

    // Identify wall nodes for the empty slot's pathfinding.
    // A node is effectively a "wall" if its data is too large to be moved into the empty slot.
    // This means `node.used > emptyNode.size` (since `emptyNode.size` is its total capacity/avail).
    // The empty slot cannot move into such a node B because Node B cannot move its data out
    // to accommodate the empty slot if B.Used > emptyNode.Size.
    def walls = new boolean[maxY + 1][maxX + 1]
    nodes.each { node ->
         if (node.used > emptyNode.size) {
             walls[node.y][node.x] = true
         }
    }

    // Optional: Print grid visualization for debugging walls, empty, and goal
    /*
    println "\nGrid Visualization:"
    (0..maxY).each { y ->
        (0..maxX).each { x ->
            def node = grid[y][x]
            def char = '.' // Normal node
            if (walls[y][x]) {
                char = '#' // Wall
            } else if (node.x == emptyNode.x && node.y == emptyNode.y) {
                 char = '_' // Empty node
            } else if (node.x == maxX && node.y == 0) {
                 char = 'G' // Goal data location
            } else if (node.x == 0 && node.y == 0) {
                 char = '(' // Target location (Node 0,0)
            }

            // Add parentheses around Node 0,0 even if it's G, _, or # for clarity
             if (node.x == 0 && node.y == 0) print '('
             print char
             if (node.x == 0 && node.y == 0) print ')' else print ' '
        }
        println ""
    }
    println ""
    */


    // Initial empty node position
    def startEmpty = new Point(emptyNode.x, emptyNode.y)

    // Goal data initial position is at (maxX, 0)

    // The strategy is to move the empty node to (maxX-1, 0), then repeatedly move the goal data
    // one step left by maneuvering the empty node around it.

    // Phase 1: Move the empty node from its start to the left of the goal data.
    // Target for empty node is (maxX - 1, 0)
    def emptyTargetPhase1 = new Point(maxX - 1, 0)

    // Calculate steps for Phase 1 using BFS
    int distPhase1 = bfs(startEmpty, emptyTargetPhase1, walls, maxX, maxY)

    // After Phase 1, empty node is at (maxX - 1, 0). Goal data is at (maxX, 0).

    // Phase 2: Move the goal data one step left for the first time.
    // G moves from (maxX, 0) to (maxX - 1, 0)
    // _ moves from (maxX - 1, 0) to (maxX, 0)
    // This takes 1 move.
    int movesPhase2 = 1

    // After Phase 2, G is at (maxX - 1, 0), _ is at (maxX, 0).
    // Phase 3: Move G from (maxX - 1, 0) to (0, 0).
    // This requires moving G (maxX - 1) steps left.
    // Each subsequent step of moving G one position left takes a fixed number of moves (5).
    // The 5 moves sequence to shift G from (x,0) to (x-1,0) when _ is at (x+1,0):
    // 1. Move _ from (x+1,0) to (x+1,1) (1 move)
    // 2. Move _ from (x+1,1) to (x,1)   (1 move)
    // 3. Move _ from (x,1)   to (x-1,1) (1 move)
    // 4. Move _ from (x-1,1) to (x-1,0) (1 move)  (Empty is now left of G)
    // 5. Move G from (x,0)   to (x-1,0), simultaneously moving _ from (x-1,0) to (x,0) (1 move)
    // Total moves per G step left = 1 + 1 + 1 + 1 + 1 = 5 moves.

    // The number of times G needs to move left from (maxX - 1, 0) down to (0, 0) is (maxX - 1) steps.
    int stepsPhase3 = maxX - 1 // Number of times G needs to shift left

    // Cost per step in Phase 3 is 5 moves.
    int movesPhase3 = stepsPhase3 * 5

    // Total steps = Phase 1 moves + Phase 2 moves + Phase 3 moves
    // distPhase1: Steps to get _ next to G for the first time.
    // movesPhase2: The first move of G.
    // movesPhase3: The cost to move G the remaining (maxX-1) steps left.
    int totalSteps = distPhase1 + movesPhase2 + movesPhase3

    println "Part Two: ${totalSteps}"
}
