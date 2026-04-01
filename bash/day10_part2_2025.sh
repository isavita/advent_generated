
#!/bin/bash

awk '
function abs(x) { return x < 0 ? -x : x }
function pivots(  i, r, col, val, c, intV, sum, pres) {
    for (i = 0; i < nB; i++) pres[i] = 0
    for (i = 0; i < nF; i++) pres[fV[i]] = fval[i]
    for (r = rank - 1; r >= 0; r--) {
        col = pcol[r]
        if (col < 0) continue
        val = mat[r, nB]
        for (c = col + 1; c < nB; c++) val -= mat[r, c] * pres[c]
        intV = int(val + (val >= 0 ? 0.5 : -0.5))
        if (abs(val - intV) > 1e-7 || intV < 0 || intV > mpr[col]) return -1
        pres[col] = intV
    }
    sum = 0
    for (i = 0; i < nB; i++) sum += pres[i]
    return sum
}
function enumr(idx, csum,   v, fv, s) {
    if (csum >= bres) return
    if (idx == nF) {
        s = pivots()
        if (s != -1 && s < bres) bres = s
        return
    }
    fv = fV[idx]
    for (v = 0; v <= mpr[fv]; v++) {
        fval[idx] = v
        enumr(idx + 1, csum + v)
    }
}
{
    nB = 0; ln = $0; nC = 0
    while (match(ln, /\([0-9,]+\)/)) {
        btn = substr(ln, RSTART + 1, RLENGTH - 2)
        bsz[nB] = split(btn, arr, ",")
        for (k = 1; k <= bsz[nB]; k++) btns[nB, k-1] = arr[k]
        nB++; ln = substr(ln, RSTART + RLENGTH)
    }
    if (match($0, /\{[0-9,]+\}/)) {
        tgt = substr($0, RSTART + 1, RLENGTH - 2)
        nC = split(tgt, tt, ",")
        for (i = 1; i <= nC; i++) ts[i-1] = tt[i]
    }
    for (j = 0; j < nC; j++) {
        for (i = 0; i < nB; i++) mat[j, i] = 0
        mat[j, nB] = ts[j]
    }
    for (i = 0; i < nB; i++)
        for (j = 0; j < bsz[i]; j++)
            if (btns[i, j] < nC) mat[btns[i, j], i] = 1
    for (i = 0; i < nC; i++) pcol[i] = -1
    row = 0
    for (col = 0; col < nB && row < nC; col++) {
        mxR = row
        for (r = row + 1; r < nC; r++)
            if (abs(mat[r, col]) > abs(mat[mxR, col])) mxR = r
        if (abs(mat[mxR, col]) < 1e-9) continue
        for (c = 0; c <= nB; c++) { t = mat[row, c]; mat[row, c] = mat[mxR, c]; mat[mxR, c] = t }
        sc = mat[row, col]
        for (c = col; c <= nB; c++) mat[row, c] /= sc
        for (r = 0; r < nC; r++) {
            if (r != row && abs(mat[r, col]) > 1e-9) {
                f = mat[r, col]
                for (c = col; c <= nB; c++) mat[r, c] -= f * mat[row, c]
            }
        }
        pcol[row] = col; row++
    }
    rank = row; nF = 0; delete isP
    for (r = 0; r < rank; r++) if (pcol[r] >= 0) isP[pcol[r]] = 1
    for (i = 0; i < nB; i++) if (!isP[i]) fV[nF++] = i
    for (i = 0; i < nB; i++) {
        m = 1e9
        for (j = 0; j < bsz[i]; j++)
            if (btns[i, j] < nC && ts[btns[i, j]] < m) m = ts[btns[i, j]]
        mpr[i] = (m == 1e9 ? 0 : m)
    }
    for (i = 0; i < nF; i++)
        for (j = i + 1; j < nF; j++)
            if (mpr[fV[i]] > mpr[fV[j]]) { t = fV[i]; fV[i] = fV[j]; fV[j] = t }
    bres = 1e9; enumr(0, 0)
    if (bres < 1e9 && bres > 0) tot += bres
    for (k in btns) delete btns[k]; for (k in bsz) delete bsz[k]
    for (k in ts) delete ts[k]; for (k in mat) delete mat[k]
    for (k in pcol) delete pcol[k]; for (k in fV) delete fV[k]
    for (k in mpr) delete mpr[k]; for (k in fval) delete fval[k]
}
END { print tot + 0 }' input.txt
