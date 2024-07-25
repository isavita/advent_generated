
import java.nio.file.Files
import java.nio.file.Paths

class RegolithReservoir {
    static void main(String[] args) {
        def cave = new HashMap<>()
        def paths = []

        // Read input from file
        Files.lines(Paths.get("input.txt")).forEach { line ->
            def points = line.split(" -> ")
            for (int i = 0; i < points.length - 1; i++) {
                def start = points[i].split(",").collect { it.toInteger() }
                def end = points[i + 1].split(",").collect { it.toInteger() }
                drawLine(cave, start, end)
            }
        }

        // Simulate sand falling
        int sandCount = simulateSand(cave)
        println(sandCount)
    }

    static void drawLine(Map cave, List<Integer> start, List<Integer> end) {
        int x1 = start[0], y1 = start[1]
        int x2 = end[0], y2 = end[1]

        if (x1 == x2) { // vertical line
            for (int y = Math.min(y1, y2); y <= Math.max(y1, y2); y++) {
                cave["$x1,$y"] = '#'
            }
        } else if (y1 == y2) { // horizontal line
            for (int x = Math.min(x1, x2); x <= Math.max(x1, x2); x++) {
                cave["$x,$y1"] = '#'
            }
        }
    }

    static int simulateSand(Map cave) {
        int sandCount = 0
        int sourceX = 500, sourceY = 0

        while (true) {
            int sandX = sourceX, sandY = sourceY
            boolean sandRested = false

            while (!sandRested) {
                if (!cave["$sandX," + (sandY + 1)]) { // down
                    sandY++
                } else if (!cave["${sandX - 1}," + (sandY + 1)]) { // down-left
                    sandX--
                    sandY++
                } else if (!cave["${sandX + 1}," + (sandY + 1)]) { // down-right
                    sandX++
                    sandY++
                } else {
                    cave["$sandX,$sandY"] = 'o' // sand rests
                    sandRested = true
                    sandCount++
                }

                // Check if sand falls into the abyss
                if (sandY > 1000) { // Adjust this value based on the cave's height
                    return sandCount
                }
            }
        }
    }
}
