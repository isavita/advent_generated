
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
    dr[1] = 0; dc[1] = 1; dr[2] = 0; dc[2] = -1; dr[3] = 1; dc[3] = 0; dr[4] = -1; dc[4] = 0
    pow2[0] = 1; for (i = 1; i <= 10; i++) pow2[i] = pow2[i - 1] * 2
}
{
    rows++
    cols = length($0)
    split($0, chars, "")
    for (i = 1; i <= cols; i++) {
        grid[rows, i] = chars[i]
        if (chars[i] ~ /[0-9]/) {
            poi_r[chars[i]] = rows
            poi_c[chars[i]] = i
            if (chars[i] > max_poi) max_poi = chars[i]
        }
    }
}
function bfs(id,    q_r, q_c, q_d, h, t, v, r, c, d, i, nr, nc) {
    h = 1; t = 1
    q_r[t] = poi_r[id]; q_c[t] = poi_c[id]; q_d[t] = 0; t++
    v[poi_r[id], poi_c[id]] = 1
    while (h < t) {
        r = q_r[h]; c = q_c[h]; d = q_d[h]; h++
        if (grid[r, c] ~ /[0-9]/) dist[id, grid[r, c]] = d
        for (i = 1; i <= 4; i++) {
            nr = r + dr[i]; nc = c + dc[i]
            if (nr >= 1 && nr <= rows && nc >= 1 && nc <= cols && grid[nr, nc] != "#" && !v[nr, nc]) {
                v[nr, nc] = 1
                q_r[t] = nr; q_c[t] = nc; q_d[t] = d + 1; t++
            }
        }
    }
}
function tsp(m, cur,    i, r, v) {
    if (m == target) return dist[cur, 0]
    if ((m, cur) in memo) return memo[m, cur]
    r = 1e9
    for (i = 0; i < n; i++) {
        if (int(m / pow2[i]) % 2 == 0) {
            v = dist[cur, i] + tsp(m + pow2[i], i)
            if (v < r) r = v
        }
    }
    return memo[m, cur] = r
}
END {
    n = max_poi + 1
    target = pow2[n] - 1
    for (i = 0; i < n; i++) bfs(i)
    print tsp(1, 0)
}
