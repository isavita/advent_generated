BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}
!gp {
    if ($0 == "" && r > 0) { gp = 1; next }
    if ($0 == "") next
    r++
    n = length($0)
    for (c = 1; c <= n; c++) {
        v = substr($0, c, 1)
        if (v != " ") {
            g[r, c] = v
            if (!ix[r]) ix[r] = c
            xx[r] = c
            if (!iy[c]) iy[c] = r
            xy[c] = r
        }
    }
    next
}
{ pt = pt $0 }
END {
    y = 1; x = 1; while (g[y, x] != ".") x++
    f = 0
    dx[0] = 1; dy[0] = 0; dx[1] = 0; dy[1] = 1; dx[2] = -1; dy[2] = 0; dx[3] = 0; dy[3] = -1
    gsub(/[RL]/, " & ", pt)
    nl = split(pt, arr)
    for (j = 1; j <= nl; j++) {
        m = arr[j]
        if (m ~ /[RL]/) {
            f = (m == "R" ? (f + 1) % 4 : (f + 3) % 4)
        } else {
            for (i = 0; i < m; i++) {
                nx = x + dx[f]; ny = y + dy[f]
                if (g[ny, nx] == "") {
                    if (f == 0) nx = ix[y]
                    else if (f == 2) nx = xx[y]
                    else if (f == 1) ny = iy[x]
                    else ny = xy[x]
                }
                if (g[ny, nx] == "#") break
                x = nx; y = ny
            }
        }
    }
    print 1000 * y + 4 * x + f
}