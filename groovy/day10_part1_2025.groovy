
import java.nio.file.Files
import java.nio.file.Paths

class Solver {
    static int bits(int x) { Integer.bitCount(x) }

    static int minWeight(int R, int C, int[][] mat) {
        int[] colPivot = new int[C]
        int pivotRow = 0
        for (int c = 0; c < C && pivotRow < R; c++) {
            int sel = -1
            for (int r = pivotRow; r < R; r++) if (mat[r][c] == 1) { sel = r; break }
            if (sel == -1) continue
            int[] tmp = mat[pivotRow]
            mat[pivotRow] = mat[sel]
            mat[sel] = tmp
            for (int r = 0; r < R; r++) if (r != pivotRow && mat[r][c] == 1) {
                for (int k = c; k <= C; k++) mat[r][k] ^= mat[pivotRow][k]
            }
            colPivot[c] = 1
            pivotRow++
        }
        for (int r = pivotRow; r < R; r++) if (mat[r][C] == 1) return -1
        List<Integer> free = []
        for (int c = 0; c < C; c++) if (colPivot[c] == 0) free << c
        int nFree = free.size()
        int best = Integer.MAX_VALUE
        int limit = 1 << nFree
        for (int mask = 0; mask < limit; mask++) {
            int[] x = new int[C]
            int cw = bits(mask)
            for (int j = 0; j < nFree; j++) if ((mask >> j) & 1) x[free[j]] = 1
            int curRow = 0
            for (int c = 0; c < C; c++) if (colPivot[c]) {
                int val = mat[curRow][C]
                for (int k = c + 1; k < C; k++) if (mat[curRow][k] == 1) val ^= x[k]
                x[c] = val
                if (val) cw++
                curRow++
            }
            if (cw < best) best = cw
        }
        best
    }

    static void main(String[] args) {
        int total = 0
        Files.readAllLines(Paths.get("input.txt")).each { line ->
            def m = line =~ /\[(.*?)\]/
            if (!m) return
            String targetStr = m[0][1]
            int R = targetStr.length()
            int[] target = new int[R]
            R.times { i -> target[i] = targetStr[i] == '#' ? 1 : 0 }
            List<int[]> buttons = []
            def btnMatcher = line =~ /\(([^)]*)\)/
            btnMatcher.each { btnMatch ->
                String inside = btnMatch[1]
                if (inside.trim()) {
                    int[] idx = inside.split(/,/)*.toInteger()
                    buttons << idx
                } else {
                    buttons << new int[0]
                }
            }
            int C = buttons.size()
            int[][] mat = new int[R][C + 1]
            R.times { r ->
                C.times { c ->
                    mat[r][c] = buttons[c].any { it == r } ? 1 : 0
                }
                mat[r][C] = target[r]
            }
            int mw = minWeight(R, C, mat)
            if (mw != -1) total += mw
        }
        println total
    }
}
