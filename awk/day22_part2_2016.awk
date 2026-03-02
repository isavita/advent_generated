
#!/usr/bin/awk -f
BEGIN { ARGV[1] = "input.txt"; ARGC = 2 }
NR > 2 {
    n = split($1, a, "-"); x = substr(a[n-1], 2) + 0; y = substr(a[n], 2) + 0
    u = $3 + 0; grid[x, y] = u
    if (u == 0) { hx = x; hy = y }
    if (x > maxX) maxX = x
}
END {
    tx = maxX - 1; q[0] = hx SUBSEP hy; t = 1; v[hx, hy] = 1; v[maxX, 0] = 1
    split("1 0 -1 0 0 1 0 -1", d)
    while (h < t) {
        split(q[h++], s, SUBSEP); cx = s[1]; cy = s[2]
        if (cx == tx && cy == 0) break
        for (i=1; i<=8; i+=2) {
            nx = cx + d[i]; ny = cy + d[i+1]
            if ((nx SUBSEP ny) in grid && grid[nx, ny] < 400 && !v[nx, ny]) {
                v[nx, ny] = 1; dist[nx, ny] = dist[cx, cy] + 1; q[t++] = nx SUBSEP ny
            }
        }
    }
    print dist[tx, 0] + 1 + 5 * (maxX - 1)
}
