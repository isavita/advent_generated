
class Main {
    static int find(int[] p, int x) {
        while (p[x] != x) {
            p[x] = p[p[x]]
            x = p[x]
        }
        x
    }

    static void unite(int[] p, int[] r, int a, int b) {
        a = find(p, a)
        b = find(p, b)
        if (a == b) return
        if (r[a] < r[b]) p[a] = b
        else if (r[a] > r[b]) p[b] = a
        else { p[b] = a; r[a]++ }
    }

    static void main(String[] args) {
        def pts = []
        new File('input.txt').eachLine { line ->
            line = line.trim()
            if (!line) return
            def parts = line.split(',')
            if (parts.size() != 3) return
            pts << [parts[0] as int, parts[1] as int, parts[2] as int]
        }
        int n = pts.size()
        if (n < 2) return

        def edges = []
        for (int i = 0; i < n; i++) {
            for (int j = i + 1; j < n; j++) {
                long dx = pts[i][0] - pts[j][0]
                long dy = pts[i][1] - pts[j][1]
                long dz = pts[i][2] - pts[j][2]
                long d = dx * dx + dy * dy + dz * dz
                edges << [u: i, v: j, d: d]
            }
        }
        edges.sort { a, b -> a.d <=> b.d }

        int[] parent = (0..<n).toArray()
        int[] rank = new int[n]
        int comps = n

        for (e in edges) {
            int ru = find(parent, e.u)
            int rv = find(parent, e.v)
            if (ru != rv) {
                unite(parent, rank, ru, rv)
                if (--comps == 1) {
                    def p1 = pts[e.u]
                    def p2 = pts[e.v]
                    println "Connected ${p1[0]},${p1[1]},${p1[2]} and ${p2[0]},${p2[1]},${p2[2]}"
                    println "Product of X coordinates: ${p1[0] as long * p2[0]}"
                    break
                }
            }
        }
    }
}
