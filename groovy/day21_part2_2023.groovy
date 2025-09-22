class StepCounterGroovy {
    static void main(String[] args) {
        new StepCounterGroovy().solve()
    }

    void solve() {
        def lines = new File("input.txt").readLines()
        if (!lines || lines.empty) {
            println "No input"
            return
        }

        int height = lines.size()
        int width = lines[0].length()

        def grid = new char[height][width]
        int startY = -1, startX = -1
        for (int i = 0; i < height; i++) {
            def line = lines[i]
            for (int j = 0; j < width; j++) {
                char ch = line.charAt(j)
                grid[i][j] = ch
                if (ch == 'S') {
                    startY = i + 1
                    startX = j + 1
                    grid[i][j] = '.'
                }
            }
        }
        if (startY == -1) {
            throw new RuntimeException("Start position S not found")
        }

        // Part 1
        int steps1 = 64
        def res1 = countReachable(grid, height, width, startY, startX, steps1, false)
        long part1 = ((Number) res1[0]).longValue()
        println "Part 1: ${part1}"

        // Part 2
        int steps2 = 26501365
        int size = width
        int offset = width / 2

        int s0 = offset
        int s1 = offset + size
        int s2 = offset + 2 * size

        def res2 = countReachable(grid, height, width, startY, startX, s2, true)
        def counts = res2[1] as java.util.Map

        if (counts == null || !counts.containsKey(s0) || !counts.containsKey(s1) || !counts.containsKey(s2)) {
            throw new RuntimeException("Failed to obtain counts for quadratic fitting")
        }

        long y0 = (counts.get(s0) as Number).longValue()
        long y1 = (counts.get(s1) as Number).longValue()
        long y2 = (counts.get(s2) as Number).longValue()

        long c = y0
        long a = (y2 - 2 * y1 + y0) / 2
        long b = (y1 - y0) - a

        long k = (steps2 - offset) / size
        long part2 = a * k * k + b * k + c

        println "Part 2: ${part2}"
    }

    static long encodePos(int y, int x) { return ((long) y << 32) | (x & 0xffffffffL) }

    static int getY(long key) { return (int) (key >>> 32) }

    static int getX(long key) { return (int) (key & 0xffffffffL) }

    static Object[] countReachable(char[][] grid, int height, int width, int startY, int startX, int targetSteps, boolean infinite) {
        def current = new java.util.HashSet<Long>()
        current.add(encodePos(startY, startX))

        def countsAtSteps = new java.util.HashMap<Integer, Long>()

        def deltas = [[-1, 0], [1, 0], [0, -1], [0, 1]] // List<List<Integer>>

        int offset = width / 2

        for (int step = 1; step <= targetSteps; step++) {
            def next = new java.util.HashSet<Long>()

            for (Long pos : current) {
                int y = getY(pos)
                int x = getX(pos)

                for (def d in deltas) {
                    int ny = y + d[0]
                    int nx = x + d[1]
                    boolean ok = false
                    if (infinite) {
                        int wrapY = ((ny - 1) % height)
                        if (wrapY < 0) wrapY += height
                        wrapY += 1
                        int wrapX = ((nx - 1) % width)
                        if (wrapX < 0) wrapX += width
                        wrapX += 1
                        if (grid[wrapY - 1][wrapX - 1] != '#') ok = true
                    } else {
                        if (ny >= 1 && ny <= height && nx >= 1 && nx <= width && grid[ny - 1][nx - 1] != '#') ok = true
                    }

                    if (ok) next.add(encodePos(ny, nx))
                }
            }

            current = next

            if (infinite) {
                if (step == offset || step == offset + width || step == offset + 2 * width) {
                    countsAtSteps.put(step, (long) current.size())
                }
            }
        }

        long finalCount = current.size()
        if (infinite) {
            return [finalCount, countsAtSteps] as Object[]
        } else {
            return [finalCount, null] as Object[]
        }
    }
}