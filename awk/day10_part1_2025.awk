
#!/usr/bin/awk -f
BEGIN {
    file = "input.txt"
    total = 0
    while ((getline line < file) > 0) {
        if (match(line, /\[[^\]]+\]/) == 0) continue
        target = substr(line, RSTART + 1, RLENGTH - 2)
        R = length(target)
        delete b
        for (i = 1; i <= R; i++) b[i - 1] = (substr(target, i, 1) == "#" ? 1 : 0)
        s = line; sub(/^.*\]/, "", s)
        C = 0; delete btn
        while (match(s, /\([0-9, ]+\)/)) {
            m = substr(s, RSTART + 1, RLENGTH - 2)
            n_p = split(m, p, /[, ]+/)
            for (k = 1; k <= n_p; k++) if (p[k] != "") btn[C, p[k] + 0] = 1
            C++; s = substr(s, RSTART + RLENGTH)
        }
        delete mat
        for (i = 0; i < R; i++) {
            for (j = 0; j < C; j++) mat[i, j] = (btn[j, i] ? 1 : 0)
            mat[i, C] = b[i]
        }
        pRow = 0; delete rowOfPivot
        for (j = 0; j < C && pRow < R; j++) {
            sel = -1
            for (i = pRow; i < R; i++) if (mat[i, j]) { sel = i; break }
            if (sel == -1) continue
            for (k = j; k <= C; k++) { tmp = mat[pRow, k]; mat[pRow, k] = mat[sel, k]; mat[sel, k] = tmp }
            for (i = 0; i < R; i++) {
                if (i != pRow && mat[i, j]) {
                    for (k = j; k <= C; k++) if (mat[pRow, k]) mat[i, k] = 1 - mat[i, k]
                }
            }
            rowOfPivot[j] = pRow; pRow++
        }
        solvable = 1
        for (i = pRow; i < R; i++) if (mat[i, C]) { solvable = 0; break }
        if (!solvable) continue
        nFree = 0; delete freeVars
        for (j = 0; j < C; j++) if (!(j in rowOfPivot)) freeVars[nFree++] = j
        minW = 1e9; limit = 2^nFree; delete pow2; delete x
        for (f = 0; f < nFree; f++) pow2[f] = 2^f
        for (i = 0; i < limit; i++) {
            cw = 0; for (f = 0; f < nFree; f++) {
                x[freeVars[f]] = int(i / pow2[f]) % 2
                if (x[freeVars[f]]) cw++
            }
            currPR = 0
            for (j = 0; j < C; j++) {
                if (j in rowOfPivot) {
                    val = mat[currPR, C]
                    for (k = j + 1; k < C; k++) if (mat[currPR, k] && x[k]) val = 1 - val
                    x[j] = val; if (val) cw++; currPR++
                }
            }
            if (cw < minW) minW = cw
        }
        if (minW != 1e9) total += minW
    }
    print total
    close(file)
}
