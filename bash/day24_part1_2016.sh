
#!/usr/bin/env bash
set -euo pipefail

awk '
function bfs(p,   h,t,r,c,d,i,nr,nc,s) {
    split(pois[p], s, " ")
    h = t = 1
    qr[t] = s[1]; qc[t] = s[2]; qd[t] = 0; t++
    delete vis
    vis[s[1], s[2]] = 1
    while (h < t) {
        r = qr[h]; c = qc[h]; d = qd[h]; h++
        if (grid[r, c] ~ /[0-9]/) dist[p, grid[r, c]] = d
        for (i = 1; i <= 4; i++) {
            nr = r + dr[i]; nc = c + dc[i]
            if ((nr, nc) in grid && grid[nr, nc] != "#" && !vis[nr, nc]) {
                vis[nr, nc] = 1
                qr[t] = nr; qc[t] = nc; qd[t] = d + 1; t++
            }
        }
    }
}

function tsp(curr, mask, count,   i,tmp,res,key) {
    if (count == n) return 0
    key = curr SUBSEP mask
    if (key in memo) return memo[key]
    res = 1e9
    for (i = 0; i < n; i++) {
        if (int(mask / p2[i]) % 2 == 0) {
            tmp = dist[curr, i] + tsp(i, mask + p2[i], count + 1)
            if (tmp < res) res = tmp
        }
    }
    return memo[key] = res
}

BEGIN {
    split("0 1 0 -1", dr)
    split("1 0 -1 0", dc)
    for (i = 0; i < 16; i++) p2[i] = 2 ^ i
}

{
    for (i = 1; i <= length($0); i++) {
        ch = substr($0, i, 1)
        grid[NR, i] = ch
        if (ch ~ /[0-9]/) {
            pois[ch] = NR " " i
            if (ch + 0 > max_p) max_p = ch + 0
        }
    }
}

END {
    n = max_p + 1
    for (i = 0; i < n; i++) bfs(i)
    print tsp(0, 1, 1)
}
' input.txt
