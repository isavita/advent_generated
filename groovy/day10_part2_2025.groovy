import java.util.Arrays

def total = 0
new File('input.txt').eachLine { line ->
    line = line.trim()
    if (!line) return
    def buttons = []
    def bm = line =~ /\(([^)]*)\)/
    bm.each { m ->
        def s = m[1].trim()
        if (!s) {
            buttons << [] as int[]
        } else {
            def arr = s.split(/,/).collect { it.trim() as int } as int[]
            buttons << arr
        }
    }
    def tm = (line =~ /\{([^}]*)\}/)
    if (tm.find()) {
        def targets = tm[0][1].split(/,/).collect { it.trim() as int } as int[]
        total += solve(buttons, targets)
    }
}
println total

def solve(buttons, targets) {
    int n = targets.size()
    int numButtons = buttons.size()
    def matrix = new double[n][numButtons + 1]
    for (int j = 0; j < n; j++) matrix[j][numButtons] = targets[j]
    for (int i = 0; i < numButtons; i++) {
        buttons[i].each { int idx ->
            if (idx < n) matrix[idx][i] = 1
        }
    }
    def pivotCol = new int[n]
    Arrays.fill(pivotCol, -1)
    int r = 0
    for (int c = 0; c < numButtons && r < n; c++) {
        int mr = r
        for (int i = r + 1; i < n; i++) if (Math.abs(matrix[i][c]) > Math.abs(matrix[mr][c])) mr = i
        if (Math.abs(matrix[mr][c]) < 1e-9) continue
        def tmp = matrix[r]; matrix[r] = matrix[mr]; matrix[mr] = tmp
        double s = matrix[r][c]
        for (int k = c; k <= numButtons; k++) matrix[r][k] /= s
        for (int i = 0; i < n; i++) if (i != r && Math.abs(matrix[i][c]) > 1e-9) {
            double f = matrix[i][c]
            for (int k = c; k <= numButtons; k++) matrix[i][k] -= f * matrix[r][k]
        }
        pivotCol[r++] = c
    }
    int rank = r
    for (int i = rank; i < n; i++) if (Math.abs(matrix[i][numButtons]) > 1e-9) return -1
    def isP = new boolean[numButtons]
    for (int i = 0; i < rank; i++) if (pivotCol[i] >= 0) isP[pivotCol[i]] = true
    def freeVars = []
    for (int i = 0; i < numButtons; i++) if (!isP[i]) freeVars << i
    def maxPresses = new int[numButtons]
    for (int i = 0; i < numButtons; i++) {
        int m = Integer.MAX_VALUE
        buttons[i].each { int j ->
            if (j < n) m = Math.min(m, targets[j])
        }
        maxPresses[i] = (m == Integer.MAX_VALUE) ? 0 : m
    }
    freeVars.sort { a, b -> maxPresses[a] <=> maxPresses[b] }
    int bestResult = Integer.MAX_VALUE
    def freeValues = new int[freeVars.size()]
    def enumerate
    enumerate = { int idx, int sum ->
        if (sum >= bestResult) return
        if (idx == freeVars.size()) {
            def res = new int[numButtons]
            for (int i = 0; i < freeVars.size(); i++) res[freeVars[i]] = freeValues[i]
            for (int i = rank - 1; i >= 0; i--) {
                int c = pivotCol[i]
                if (c < 0) continue
                double v = matrix[i][numButtons]
                for (int k = c + 1; k < numButtons; k++) v -= matrix[i][k] * res[k]
                int iv = (int) Math.round(v)
                if (Math.abs(v - iv) > 1e-6 || iv < 0 || iv > maxPresses[c]) return
                res[c] = iv
            }
            int cur = 0
            res.each { cur += it }
            if (cur < bestResult) bestResult = cur
        } else {
            int fv = freeVars[idx]
            for (int v = 0; v <= maxPresses[fv]; v++) {
                freeValues[idx] = v
                enumerate(idx + 1, sum + v)
            }
        }
    }
    enumerate(0, 0)
    return bestResult == Integer.MAX_VALUE ? -1 : bestResult
}