
import java.util.Queue
import java.util.LinkedList

def solvePart1(grid, start, end) {
    return bfs(grid, start, end)
}

def solvePart2(grid, end) {
    def starts = []
    for (int i = 0; i < grid.size(); i++) {
        for (int j = 0; j < grid[0].size(); j++) {
            if (grid[i][j] == 'a') {
                starts.add([i, j])
            }
        }
    }
    def minSteps = Integer.MAX_VALUE
    for (def start : starts) {
        def steps = bfs(grid, start, end)
        if (steps != -1) {
            minSteps = Math.min(minSteps, steps)
        }
    }
    return minSteps
}

def bfs(grid, start, end) {
    def rows = grid.size()
    def cols = grid[0].size()
    def visited = new boolean[rows][cols]
    def queue = new LinkedList<>()
    queue.offer([start[0], start[1], 0]) // row, col, steps
    visited[start[0]][start[1]] = true

    def directions = [[0, 1], [0, -1], [1, 0], [-1, 0]]

    while (!queue.isEmpty()) {
        def current = queue.poll()
        def row = current[0]
        def col = current[1]
        def steps = current[2]

        if (row == end[0] && col == end[1]) {
            return steps
        }

        for (def dir : directions) {
            def newRow = row + dir[0]
            def newCol = col + dir[1]

            if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols && !visited[newRow][newCol]) {
                def currentHeight = grid[row][col]
                def nextHeight = grid[newRow][newCol]

                if (currentHeight == 'S') currentHeight = 'a'
                if (nextHeight == 'E') nextHeight = 'z'

                if (nextHeight.charAt(0) <= currentHeight.charAt(0) + 1) {
                    queue.offer([newRow, newCol, steps + 1])
                    visited[newRow][newCol] = true
                }
            }
        }
    }
    return -1 // No path found
}

def main() {
    def grid = []
    def start = null
    def end = null

    new File("input.txt").eachLine { line ->
        grid.add(line.toList().collect { it.toString() })
    }

    for (int i = 0; i < grid.size(); i++) {
        for (int j = 0; j < grid[0].size(); j++) {
            if (grid[i][j] == 'S') {
                start = [i, j]
            }
            if (grid[i][j] == 'E') {
                end = [i, j]
            }
        }
    }

    def part1Result = solvePart1(grid, start, end)
    println "Part 1: ${part1Result}"

    def part2Result = solvePart2(grid, end)
    println "Part 2: ${part2Result}"
}

main()
