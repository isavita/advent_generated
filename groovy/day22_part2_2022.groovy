import java.nio.file.Files
import java.nio.file.Paths

class Main {
    static void main(String[] args) {
        String input = new String(Files.readAllBytes(Paths.get("input.txt")), "UTF-8")
        def parts = input.split(/\r?\n\r?\n/, 2)
        def mapLines = parts[0].split(/\r?\n/)
        def path = parts[1].trim()
        int rows = mapLines.length
        int cols = mapLines.collect { it.length() }.max()
        char[][] map = new char[rows][cols]
        for (int r = 0; r < rows; r++) {
            def line = mapLines[r]
            for (int c = 0; c < cols; c++) {
                map[r][c] = c < line.length() ? line.charAt(c) : ' '
            }
        }
        def instructions = path.findAll(/\d+|[LR]/).collect { it.isInteger() ? it.toInteger() : it }
        int row = 0
        int col = new String(map[0]).indexOf('.')
        int dir = 0
        def getFace = { int r, int c ->
            if (r >= 0 && r < 50 && c >= 50 && c < 100) 1
            else if (r >= 0 && r < 50 && c >= 100 && c < 150) 2
            else if (r >= 50 && r < 100 && c >= 50 && c < 100) 3
            else if (r >= 100 && r < 150 && c >= 0 && c < 50) 4
            else if (r >= 100 && r < 150 && c >= 50 && c < 100) 5
            else if (r >= 150 && r < 200 && c >= 0 && c < 50) 6
            else null
        }
        def wrap = { int r, int c, int d ->
            def face = getFace(r, c)
            Integer nr = null, nc = null, nd = null
            if (face == 1) {
                if (d == 3 && r == 0) { nr = c - 50 + 150; nc = 0; nd = 0 }
                else if (d == 2 && c == 50) { nr = 149 - r; nc = 0; nd = 0 }
            } else if (face == 2) {
                if (d == 3 && r == 0) { nr = 199; nc = c - 100; nd = 3 }
                else if (d == 0 && c == 149) { nr = 149 - r; nc = 99; nd = 2 }
                else if (d == 1 && r == 49) { nr = c - 100 + 50; nc = 99; nd = 2 }
            } else if (face == 3) {
                if (d == 0 && c == 99) { nr = 49; nc = r - 50 + 100; nd = 3 }
                else if (d == 2 && c == 50) { nr = 100; nc = r - 50; nd = 1 }
            } else if (face == 4) {
                if (d == 2 && c == 0) { nr = 149 - r; nc = 50; nd = 0 }
                else if (d == 3 && r == 100) { nr = c + 50; nc = 50; nd = 0 }
            } else if (face == 5) {
                if (d == 0 && c == 99) { nr = 149 - r; nc = 149; nd = 2 }
                else if (d == 1 && r == 149) { nr = c - 50 + 150; nc = 49; nd = 2 }
            } else if (face == 6) {
                if (d == 0 && c == 49) { nr = 149; nc = r - 150 + 50; nd = 3 }
                else if (d == 1 && r == 199) { nr = 0; nc = c + 100; nd = 1 }
                else if (d == 2 && c == 0) { nr = 0; nc = r - 150 + 50; nd = 1 }
            }
            [row: nr, col: nc, dir: nd]
        }
        for (inst in instructions) {
            if (inst instanceof Integer) {
                int steps = inst
                stepLoop:
                for (int s = 0; s < steps; s++) {
                    int nr = row, nc = col, nd = dir
                    if (dir == 0) nc++
                    else if (dir == 1) nr++
                    else if (dir == 2) nc--
                    else if (dir == 3) nr--
                    if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && map[nr][nc] != ' ') {
                        if (map[nr][nc] == '#') break
                        row = nr; col = nc
                    } else {
                        def res = wrap(row, col, dir)
                        nr = res.row; nc = res.col; nd = res.dir
                        if (nr == null || nc == null || nd == null) throw new RuntimeException("Invalid wrap")
                        if (map[nr][nc] == '#') break
                        if (map[nr][nc] == '.') { row = nr; col = nc; dir = nd }
                        else throw new RuntimeException("Invalid map at (${nr},${nc})")
                    }
                }
            } else if (inst == 'L') {
                dir = (dir + 3) % 4
            } else if (inst == 'R') {
                dir = (dir + 1) % 4
            }
        }
        long password = 1000L * (row + 1) + 4L * (col + 1) + dir
        println(password)
    }
}