
function bfs(p, h, t, r, c, d, i, nr, nc, s, q_r, q_c, q_d, v) {
    split(pois[p], s)
    h = t = 1
    q_r[t] = s[1]; q_c[t] = s[2]; q_d[t] = 0; t++
    v[s[1], s[2]] = 1
    while (h < t) {
        r = q_r[h]; c = q_c[h]; d = q_d[h]; h++
        if (grid[r, c] ~ /[0-9]/) dist[p, grid[r, c]] = d
        for (i = 1; i <= 4; i++) {
            nr = r + dr[i]; nc = c + dc[i]
            if ((nr, nc) in grid && grid[nr, nc] != "#" && !v[nr, nc]) {
                v[nr, nc] = 1
                q_r[t] = nr; q_c[t] = nc; q_d[t] = d + 1; t++
            }
        }
    }
}

function tsp(curr, mask, count, i, res, tmp) {
    if (count == n) return 0
    if ((curr, mask) in memo) return memo[curr, mask]
    res = 1e9
    for (i = 0; i < n; i++) {
        if (int(mask / p2[i]) % 2 == 0) {
            tmp = dist[curr, i] + tsp(i, mask + p2[i], count + 1)
            if (tmp < res) res = tmp
        }
    }
    return memo[curr, mask] = res
}

BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
    split("0 0 1 -1", dr)
    split("1 -1 0 0", dc)
    for (i = 0; i < 16; i++) p2[i] = 2^i
}

{
    for (i = 1; i <= length($0); i++) {
        char = substr($0, i, 1)
        grid[NR, i] = char
        if (char ~ /[0-9]/) {
            pois[char] = NR " " i
            if (char + 0 > max_p) max_p = char + 0
        }
    }
}

END {
    n = max_p + 1
    for (i = 0; i < n; i++) bfs(i)
    print tsp(0, 1, 1)
}

