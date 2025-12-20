
import java.math.BigInteger

class TimelineCounter {
    static void main(String[] args) {
        def file = new File('input.txt')
        if (!file.exists()) {
            System.err.println('File not found')
            System.exit(1)
        }
        def grid = file.readLines().findAll { it }
        if (grid.isEmpty()) {
            println '0'
            return
        }
        int height = grid.size()
        int width = grid[0].length()
        int startX = -1, startY = -1
        outer:
        for (int y = 0; y < height; y++) {
            def row = grid[y]
            for (int x = 0; x < row.length(); x++) {
                if (row.charAt(x) == 'S') {
                    startX = x
                    startY = y
                    break outer
                }
            }
        }
        if (startX == -1) {
            System.err.println("Start point 'S' not found")
            System.exit(1)
        }
        def counts = [(startX): BigInteger.ONE]
        for (int y = startY; y < height; y++) {
            def next = [:]
            counts.each { x, cnt ->
                boolean splitter = x >= 0 && x < width && grid[y].charAt(x) == '^'
                if (splitter) {
                    next[x - 1] = (next.get(x - 1, BigInteger.ZERO)).add(cnt)
                    next[x + 1] = (next.get(x + 1, BigInteger.ZERO)).add(cnt)
                } else {
                    next[x] = (next.get(x, BigInteger.ZERO)).add(cnt)
                }
            }
            counts = next
        }
        def total = counts.values().inject(BigInteger.ZERO) { a, b -> a.add(b) }
        println total
    }
}
