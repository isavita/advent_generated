
class Main {
    static void main(String[] args) {
        def lines = new File('input.txt').readLines().findAll { it }
        if (!lines) { println 'Empty grid'; return }

        int height = lines.size()
        int width = lines[0].size()
        int startX = -1, startY = -1
        lines.eachWithIndex { row, y ->
            int x = row.indexOf('S')
            if (x != -1) { startX = x; startY = y; return }
        }
        if (startX == -1) { System.err.println("Start point 'S' not found"); return }

        Set<Integer> active = [startX] as Set
        int totalSplits = 0

        for (int y = startY; y < height && !active.isEmpty(); y++) {
            Set<Integer> next = [] as Set
            active.each { x ->
                if (x < 0 || x >= width) return
                char cell = lines[y][x]
                if (cell == '^') {
                    totalSplits++
                    if (x - 1 >= 0) next << (x - 1)
                    if (x + 1 < width) next << (x + 1)
                } else {
                    next << x
                }
            }
            active = next
        }
        println "Total times the beam is split: $totalSplits"
    }
}
