
import java.util.LinkedList
import java.util.Queue

class BlizzardBasin {

    static void main(String[] args) {
        def grid = new File("input.txt").readLines()
        def width = grid[0].length()
        def height = grid.size()
        def start = [0, grid[0].indexOf('.')]
        def goal = [height - 1, grid[height - 1].indexOf('.')]

        def solve = { startPos, endPos, initialTime ->
            def blizzards = [:]
            grid.eachWithIndex { row, y ->
                row.eachWithIndex { cell, x ->
                    if (['>', '<', '^', 'v'].contains(cell)) {
                        blizzards[[y, x]] = cell
                    }
                }
            }

            def getBlizzardPositions = { time ->
                def currentBlizzards = [:]
                blizzards.each { pos, dir ->
                    def y = pos[0]
                    def x = pos[1]
                    def newY = y
                    def newX = x

                    switch (dir) {
                        case '>': newX = (x - 1 + time) % (width - 2) + 1; break
                        case '<': newX = (x - 1 - time % (width - 2) + (width - 2)) % (width - 2) + 1; break
                        case '^': newY = (y - 1 - time % (height - 2) + (height - 2)) % (height - 2) + 1; break
                        case 'v': newY = (y - 1 + time) % (height - 2) + 1; break
                    }
                    currentBlizzards[[newY, newX]] = true
                }
                return currentBlizzards
            }

            def isValid = { y, x ->
                y >= 0 && y < height && x >= 0 && x < width && grid[y][x] != '#'
            }

            def bfs = { startY, startX, startTime ->
                def visited = [:]
                def queue = new LinkedList<>()
                queue.add([startY, startX, startTime])
                visited[[startY, startX, startTime]] = true

                while (!queue.isEmpty()) {
                    def current = queue.poll()
                    def y = current[0]
                    def x = current[1]
                    def time = current[2]

                    if (y == endPos[0] && x == endPos[1]) {
                        return time
                    }

                    def nextTimeBlizzards = getBlizzardPositions(time + 1)

                    def moves = [[0, 0], [0, 1], [0, -1], [1, 0], [-1, 0]]

                    moves.each { move ->
                        def nextY = y + move[0]
                        def nextX = x + move[1]

                        if (isValid(nextY, nextX) && !nextTimeBlizzards.containsKey([nextY, nextX]) && !visited.containsKey([nextY, nextX, time + 1])) {
                            queue.add([nextY, nextX, time + 1])
                            visited[[nextY, nextX, time + 1]] = true
                        }
                    }
                }
                return -1 // Should not happen
            }
            return bfs(startPos[0], startPos[1], initialTime)
        }


        def trip1 = solve(start, goal, 0)
        println "Part 1: ${trip1}"

        def trip2 = solve(goal, start, trip1)
        def trip3 = solve(start, goal, trip2)

        println "Part 2: ${trip3}"
    }
}
