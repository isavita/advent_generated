
#!/bin/bash
awk 'BEGIN { FS="" }
{
    for (i = 1; i <= NF; i++) {
        if ($i == "#") v[i, NR] = 1
        if ($i == "S") { x = i; y = NR }
    }
    w = NF; h = NR
}
END {
    qx[1] = x; qy[1] = y; d[x, y] = 0; l = 1; r = 2
    while (l < r) {
        cx = qx[l]; cy = qy[l++]; cd = d[cx, cy]
        if (cd < 64) {
            for (i = 1; i <= 4; i++) {
                nx = cx + (i == 1) - (i == 2)
                ny = cy + (i == 3) - (i == 4)
                if (nx > 0 && nx <= w && ny > 0 && ny <= h && !v[nx, ny] && !((nx, ny) in d)) {
                    d[nx, ny] = cd + 1
                    qx[r] = nx; qy[r++] = ny
                }
            }
        }
    }
    for (k in d) if (d[k] % 2 == 0) a++
    print a
}' input.txt
