
class Main {
    static void main(String[] args) {
        def lines = new File('input.txt').readLines().findAll { it }
        if (!lines) { println 'Empty grid'; return }
        int rows = lines.size()
        int cols = lines[0].size()
        def grid = lines.collect { it.toCharArray() }
        int accessible = 0
        for (int y = 0; y < rows; y++) {
            for (int x = 0; x < cols; x++) {
                if (grid[y][x] != '@') continue
                int neighbors = 0
                for (int dy = -1; dy <= 1; dy++) {
                    for (int dx = -1; dx <= 1; dx++) {
                        if (dx == 0 && dy == 0) continue
                        int ny = y + dy, nx = x + dx
                        if (nx >= 0 && nx < cols && ny >= 0 && ny < rows && grid[ny][nx] == '@') neighbors++
                    }
                }
                if (neighbors < 4) accessible++
            }
        }
        println "Number of accessible rolls of paper: $accessible"
    }
}
