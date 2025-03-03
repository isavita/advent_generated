
import java.util.PriorityQueue

class Day22ModeMaze {

    static final int MOD = 20183
    static final int ROCKY = 0
    static final int WET = 1
    static final int NARROW = 2

    static final int TORCH = 0
    static final int CLIMBING_GEAR = 1
    static final int NEITHER = 2

    static int depth
    static int targetX
    static int targetY

    static int[][] geologicIndex
    static int[][] erosionLevel
    static int[][] regionType

    static void calculateErosionLevels(int extra) {
        geologicIndex = new int[targetY + 1 + extra][targetX + 1 + extra]
        erosionLevel = new int[targetY + 1 + extra][targetX + 1 + extra]
        regionType = new int[targetY + 1 + extra][targetX + 1 + extra]


        for (int y = 0; y < targetY + 1 + extra; y++) {
            for (int x = 0; x < targetX + 1 + extra; x++) {
                if ((x == 0 && y == 0) || (x == targetX && y == targetY)) {
                    geologicIndex[y][x] = 0
                } else if (y == 0) {
                    geologicIndex[y][x] = x * 16807
                } else if (x == 0) {
                    geologicIndex[y][x] = y * 48271
                } else {
                    geologicIndex[y][x] = erosionLevel[y][x - 1] * erosionLevel[y - 1][x]
                }
                erosionLevel[y][x] = (geologicIndex[y][x] + depth) % MOD
                regionType[y][x] = erosionLevel[y][x] % 3
            }
        }
    }

    static int calculateRiskLevel() {
        int riskLevel = 0
        for (int y = 0; y <= targetY; y++) {
            for (int x = 0; x <= targetX; x++) {
                riskLevel += regionType[y][x]
            }
        }
        return riskLevel
    }


    static int fewestMinutesToTarget() {
        // Dijkstra's algorithm.  State is (x, y, tool).
        PriorityQueue<State> queue = new PriorityQueue<>(Comparator.comparingInt { it.minutes })
        queue.offer(new State(0, 0, TORCH, 0))

        def visited = new HashSet<String>() //x.y.tool
        def dist = [:] // Distance from start to this state. key format = state

        dist["0.0.$TORCH"] = 0

        while (!queue.isEmpty()) {
            State current = queue.poll()

            if (current.x == targetX && current.y == targetY && current.tool == TORCH) {
                return current.minutes
            }

            String currentStateStr = "${current.x}.${current.y}.${current.tool}"

            if (visited.contains(currentStateStr)) continue
            visited.add(currentStateStr)
            
            // Try switching tools
            for (int newTool in [TORCH, CLIMBING_GEAR, NEITHER]) {
                if (newTool != current.tool && canUseTool(current.x, current.y, newTool)) {
                    String nextStateStr = "${current.x}.${current.y}.${newTool}"
                    int newDist = current.minutes + 7

                    if(dist.get(nextStateStr, Integer.MAX_VALUE) > newDist) {
                        dist[nextStateStr] = newDist
                        queue.offer(new State(current.x, current.y, newTool, newDist))
                    }
                }
            }
            // Try moving
            int[][] moves = [[0, 1], [0, -1], [1, 0], [-1, 0]]
            for (int[] move in moves) {
                int nx = current.x + move[0]
                int ny = current.y + move[1]

                if (nx >= 0 && ny >= 0 && nx < regionType[0].length && ny < regionType.length && canUseTool(nx, ny, current.tool)) {

                    int newDist = current.minutes + 1
                    String nextStateStr = "${nx}.${ny}.${current.tool}"

                    if(dist.get(nextStateStr, Integer.MAX_VALUE) > newDist) {
                        dist[nextStateStr] = newDist
                        queue.offer(new State(nx, ny, current.tool, newDist))
                    }
                }
            }
        }
        return -1 // Should never happen, given constraints
    }

    static boolean canUseTool(int x, int y, int tool) {
        int type = regionType[y][x]
        return (type == ROCKY && (tool == TORCH || tool == CLIMBING_GEAR)) ||
                (type == WET && (tool == CLIMBING_GEAR || tool == NEITHER)) ||
                (type == NARROW && (tool == TORCH || tool == NEITHER))
    }


    static void main(String[] args) {
        File inputFile = new File("input.txt")
        if (!inputFile.exists()) {
            println "Error: input.txt does not exist."
            return
        }
        List<String> lines = inputFile.readLines()
        depth = Integer.parseInt(lines[0].split(":")[1].trim())
        String[] targetCoords = lines[1].split(":")[1].trim().split(",")
        targetX = Integer.parseInt(targetCoords[0])
        targetY = Integer.parseInt(targetCoords[1])

        calculateErosionLevels(50) //extra for part 2 Dijkstra

        println "Part 1: ${calculateRiskLevel()}"
        println "Part 2: ${fewestMinutesToTarget()}"

    }

    static class State {
        int x
        int y
        int tool
        int minutes

        State(int x, int y, int tool, int minutes) {
            this.x = x
            this.y = y
            this.tool = tool
            this.minutes = minutes
        }
    }
}
