
function abs(x) { return x < 0 ? -x : x }

function computePivots(cur_sum,    r, col, c, v, iv, i, s) {
    for (i = 0; i < btn_len; i++) _res[i] = 0
    for (i = 1; i <= free_count; i++) _res[freeVars[i]] = freeVals[i]
    s = cur_sum
    for (r = rank - 1; r >= 0; r--) {
        col = pivCol[r]
        v = mat[r, btn_len]
        for (c = col + 1; c < btn_len; c++) v -= mat[r, c] * _res[c]
        iv = int(v + (v < 0 ? -0.5 : 0.5))
        if (abs(v - iv) > 1e-6 || iv < 0 || iv > maxPresses[col]) return 9e15
        _res[col] = iv
        s += iv
    }
    return s
}

function enumerate(idx, cur,    fv, limit, v, s) {
    if (cur >= best) return
    if (idx > free_count) {
        s = computePivots(cur)
        if (s < best) best = s
        return
    }
    fv = freeVars[idx]
    limit = maxPresses[fv]
    for (v = 0; v <= limit; v++) {
        freeVals[idx] = v
        enumerate(idx + 1, cur + v)
    }
}

function solve_system(   i, j, k, r, c, row, col, maxRow, tmp, scale, f, isP, idxs, n_indices, idx, t_var) {
    delete mat
    for (i = 0; i < target_len; i++) mat[i, btn_len] = targets[i]
    for (i = 0; i < btn_len; i++) {
        n_indices = split(buttons[i], idxs, ",")
        for (k = 1; k <= n_indices; k++) {
            if (idxs[k] != "") {
                idx = idxs[k]; if (idx < target_len) mat[idx, i] = 1
            }
        }
    }
    delete pivCol; row = 0
    for (col = 0; col < btn_len && row < target_len; col++) {
        maxRow = row
        for (r = row + 1; r < target_len; r++)
            if (abs(mat[r, col]) > abs(mat[maxRow, col])) maxRow = r
        if (abs(mat[maxRow, col]) < 1e-9) continue
        for (c = col; c <= btn_len; c++) {
            tmp = mat[row, c]; mat[row, c] = mat[maxRow, c]; mat[maxRow, c] = tmp
        }
        scale = mat[row, col]
        for (c = col; c <= btn_len; c++) mat[row, c] /= scale
        for (r = 0; r < target_len; r++) {
            if (r != row && abs(mat[r, col]) > 1e-9) {
                f = mat[r, col]
                for (c = col; c <= btn_len; c++) mat[r, c] -= f * mat[row, c]
            }
        }
        pivCol[row++] = col
    }
    rank = row
    for (r = rank; r < target_len; r++) if (abs(mat[r, btn_len]) > 1e-9) return -1
    free_count = 0; delete freeVars
    for (i = 0; i < btn_len; i++) {
        isP = 0
        for (r = 0; r < rank; r++) if (pivCol[r] == i) isP = 1
        if (!isP) freeVars[++free_count] = i
    }
    delete maxPresses
    for (i = 0; i < btn_len; i++) {
        limit = 9e15; n_indices = split(buttons[i], idxs, ","); found = 0
        for (k = 1; k <= n_indices; k++) {
            if (idxs[k] != "") {
                idx = idxs[k]; if (idx < target_len) {
                    if (targets[idx] < limit) limit = targets[idx]; found = 1
                }
            }
        }
        maxPresses[i] = found ? limit : 0
    }
    for (i = 1; i < free_count; i++) {
        for (j = i + 1; j <= free_count; j++) {
            if (maxPresses[freeVars[i]] > maxPresses[freeVars[j]]) {
                t_var = freeVars[i]; freeVars[i] = freeVars[j]; freeVars[j] = t_var
            }
        }
    }
    best = 9e15; delete freeVals; enumerate(1, 0)
    return (best == 9e15 ? -1 : best)
}

BEGIN {
    total = 0
    input = "input.txt"
    while ((getline line < input) > 0) {
        if (line !~ /\(.*\)/) continue
        delete buttons; delete targets; btn_len = 0; target_len = 0; s = line
        while (match(s, /\([^)]*\)/)) {
            content = substr(s, RSTART + 1, RLENGTH - 2); gsub(/ /, "", content)
            buttons[btn_len++] = content; s = substr(s, RSTART + RLENGTH)
        }
        if (match(line, /\{[^}]*\}/)) {
            content = substr(line, RSTART + 1, RLENGTH - 2); gsub(/ /, "", content)
            target_len = split(content, targets, ",")
            for (i = 1; i <= target_len; i++) targets[i-1] = targets[i]
        }
        total += solve_system()
    }
    print total
}

