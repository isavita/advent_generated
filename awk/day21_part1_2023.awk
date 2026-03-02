BEGIN {
    FS = ""
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    h++
    w = NF
    for (x = 1; x <= NF; x++) {
        if ($x == "#") v[x, h] = 1
        if ($x == "S") { sx = x; sy = h }
    }
}
END {
    qx[1] = sx; qy[1] = sy; d[sx, sy] = 0; ql = 1; qr = 2
    dx[1] = 1; dx[2] = -1; dx[3] = 0; dx[4] = 0
    dy[1] = 0; dy[2] = 0; dy[3] = 1; dy[4] = -1
    while (ql < qr) {
        x = qx[ql]; y = qy[ql]; ql++
        cd = d[x, y]
        if (cd < 64) {
            for (i = 1; i <= 4; i++) {
                nx = x + dx[i]; ny = y + dy[i]
                if (nx > 0 && nx <= w && ny > 0 && ny <= h && !v[nx, ny] && !((nx, ny) in d)) {
                    d[nx, ny] = cd + 1
                    qx[qr] = nx; qy[qr] = ny; qr++
                }
            }
        }
    }
    for (k in d) if (d[k] % 2 == 0) r++
    print r
}