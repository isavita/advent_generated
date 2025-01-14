
import java.util.LinkedList
import java.util.Queue

def solvePart1(List<String> input, int gridSize) {
    def grid = new boolean[gridSize][gridSize]
    def start = [0, 0]
    def end = [gridSize - 1, gridSize - 1]
    
    input.take(1024).each { line ->
        def (x, y) = line.split(',').collect { it.toInteger() }
        grid[y][x] = true
    }

    return bfs(grid, start, end)
}

def solvePart2(List<String> input, int gridSize) {
    def grid = new boolean[gridSize][gridSize]
    def start = [0, 0]
    def end = [gridSize - 1, gridSize - 1]

    for (int i = 0; i < input.size(); i++) {
        def (x, y) = input[i].split(',').collect { it.toInteger() }
        grid[y][x] = true
        if (bfs(grid, start, end) == -1) {
            return "${x},${y}"
        }
    }
    return "No blocking byte found"
}

def bfs(boolean[][] grid, List<Integer> start, List<Integer> end) {
    def gridSize = grid.length
    def visited = new boolean[gridSize][gridSize]
    def queue = new LinkedList<List<Integer>>()
    queue.add([start[0], start[1], 0])
    visited[start[1]][start[0]] = true

    while (!queue.isEmpty()) {
        def current = queue.poll()
        def x = current[0]
        def y = current[1]
        def steps = current[2]

        if (x == end[0] && y == end[1]) {
            return steps
        }

        def directions = [[0, 1], [0, -1], [1, 0], [-1, 0]]
        directions.each { dir ->
            def newX = x + dir[0]
            def newY = y + dir[1]

            if (newX >= 0 && newX < gridSize && newY >= 0 && newY < gridSize &&
                !grid[newY][newX] && !visited[newY][newX]) {
                queue.add([newX, newY, steps + 1])
                visited[newY][newX] = true
            }
        }
    }
    return -1
}

def input = new File('input.txt').readLines()
def gridSize = 71

println solvePart1(input, gridSize)
println solvePart2(input, gridSize)
