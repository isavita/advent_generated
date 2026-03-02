BEGIN {
    if (ARGC == 1) ARGV[ARGC++] = "input.txt"
    dr[1]=1; dr[2]=-1; dr[3]=0; dr[4]=0
    dc[1]=0; dc[2]=0; dc[3]=1; dc[4]=-1
}
{
    l[NR] = $0
    if (length($0) > mc) mc = length($0)
}
END {
    mr = NR
    for (r = 1; r <= mr; r++)
        for (c = 1; c <= mc; c++)
            g[r, c] = substr(l[r], c, 1)
    for (r = 1; r <= mr; r++) {
        for (c = 1; c <= mc; c++) {
            if (g[r, c] ~ /[A-Z]/) {
                if (g[r, c+1] ~ /[A-Z]/) {
                    n = g[r, c] g[r, c+1]; pr = -1
                    if (g[r, c-1] == ".") { pr = r; pc = c-1 }
                    else if (g[r, c+2] == ".") { pr = r; pc = c+2 }
                    if (pr != -1) {
                        if (n == "AA") { sr = pr; sc = pc }
                        else if (n == "ZZ") { er = pr; ec = pc }
                        else if (n in p_r) {
                            tr[p_r[n], p_c[n]] = pr; tc[p_r[n], p_c[n]] = pc
                            tr[pr, pc] = p_r[n]; tc[pr, pc] = p_c[n]
                        } else { p_r[n] = pr; p_c[n] = pc }
                    }
                }
                if (g[r+1, c] ~ /[A-Z]/) {
                    n = g[r, c] g[r+1, c]; pr = -1
                    if (g[r-1, c] == ".") { pr = r-1; pc = c }
                    else if (g[r+2, c] == ".") { pr = r+2; pc = c }
                    if (pr != -1) {
                        if (n == "AA") { sr = pr; sc = pc }
                        else if (n == "ZZ") { er = pr; ec = pc }
                        else if (n in p_r) {
                            tr[p_r[n], p_c[n]] = pr; tc[p_r[n], p_c[n]] = pc
                            tr[pr, pc] = p_r[n]; tc[pr, pc] = p_c[n]
                        } else { p_r[n] = pr; p_c[n] = pc }
                    }
                }
            }
        }
    }
    h = 0; t = 0; qx[++t] = sr; qy[t] = sc; d[sr, sc] = 0
    while (h < t) {
        h++; r = qx[h]; c = qy[h]
        if (r == er && c == ec) { print d[r, c]; exit }
        for (i = 1; i <= 4; i++) {
            nr = r + dr[i]; nc = c + dc[i]
            if (g[nr, nc] == ".") {
                if (!((nr, nc) in d)) {
                    d[nr, nc] = d[r, c] + 1; qx[++t] = nr; qy[t] = nc
                }
            } else if (g[nr, nc] ~ /[A-Z]/ && (r, c) in tr) {
                rr = tr[r, c]; cc = tc[r, c]
                if (!((rr, cc) in d)) {
                    d[rr, cc] = d[r, c] + 1; qx[++t] = rr; qy[t] = cc
                }
            }
        }
    }
}