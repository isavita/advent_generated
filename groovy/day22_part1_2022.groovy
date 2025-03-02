
class Solution {

    static void main(String[] args) {
        def lines = new File("input.txt").readLines()
        def map = []
        def path = ""
        def mapEnd = 0

        // Parse input
        for (int i = 0; i < lines.size(); i++) {
            if (lines[i].isEmpty()) {
                mapEnd = i
                break
            }
            map << lines[i]
        }
        path = lines[mapEnd + 1]


        // Find starting position
        int row = 0
        int col = map[0].indexOf('.')
        int facing = 0 // 0: right, 1: down, 2: left, 3: up

        // Process path instructions
        def instructions = path.split("(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)")

        for (def instruction : instructions) {
            if (instruction.matches("\\d+")) {
                // Move
                def steps = Integer.parseInt(instruction)
                for (int i = 0; i < steps; i++) {
                    def nextRow = row
                    def nextCol = col
                    def nextFacing = facing

                    switch (facing) {
                        case 0: // Right
                            nextCol++
                            if (nextCol >= map[row].length() || map[row][nextCol] == ' ') {
                                // Wrap around
                                nextCol = 0
                                while (nextCol < map[row].length() && map[row][nextCol] == ' ') {
                                    nextCol++
                                }
                            }
                            break
                        case 1: // Down
                            nextRow++
                            if (nextRow >= map.size() || nextCol >= map[nextRow].length() || map[nextRow][nextCol] == ' ') {
                                // Wrap around
                                nextRow = 0
                                while (nextRow < map.size() && (nextCol >= map[nextRow].length() || map[nextRow][nextCol] == ' ')) {
                                    nextRow++
                                }
                            }
                            break
                        case 2: // Left
                            nextCol--
                            if (nextCol < 0 || map[row][nextCol] == ' ') {
                                // Wrap around
                                nextCol = map[row].length() - 1
                                while (nextCol >= 0 && map[row][nextCol] == ' ') {
                                    nextCol--
                                }
                            }
                            break
                        case 3: // Up
                            nextRow--
                            if (nextRow < 0 || nextCol >= map[nextRow].length() || map[nextRow][nextCol] == ' ') {
                                // Wrap around
                                nextRow = map.size() - 1
                                while (nextRow >= 0 && (nextCol >= map[nextRow].length() || map[nextRow][nextCol] == ' ')) {
                                    nextRow--
                                }
                            }
                            break
                    }


                    if (nextCol < map[nextRow].length() && map[nextRow][nextCol] == '#') {
                        break // Hit a wall
                    }

                    row = nextRow
                    col = nextCol
                }
            } else {
                // Turn
                if (instruction == 'R') {
                    facing = (facing + 1) % 4
                } else { // 'L'
                    facing = (facing + 3) % 4
                }
            }
        }

        // Calculate final password
        def finalPassword = 1000 * (row + 1) + 4 * (col + 1) + facing
        println finalPassword
    }
}
