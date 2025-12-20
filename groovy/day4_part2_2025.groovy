
def rows = []
new File('input.txt').eachLine { line ->
    if (line) rows << line.toCharArray()
}
if (!rows) { println 'Total rolls removed: 0'; return }

int rCount = rows.size()
int cCount = rows[0].size()
int total = 0
int[][] dirs = [[-1,-1],[-1,0],[-1,1],[0,-1],[0,1],[1,-1],[1,0],[1,1]]

while (true) {
    List<int[]> rem = []
    for (int r = 0; r < rCount; r++) {
        for (int c = 0; c < cCount; c++) {
            if (rows[r][c] != '@') continue
            int n = 0
            for (d in dirs) {
                int nr = r + d[0], nc = c + d[1]
                if (nr >= 0 && nr < rCount && nc >= 0 && nc < cCount && rows[nr][nc] == '@')
                    n++
            }
            if (n < 4) rem << [r, c]
        }
    }
    if (!rem) break
    total += rem.size()
    rem.each { rows[it[0]][it[1]] = '.' }
}
println "Total rolls removed: $total"
