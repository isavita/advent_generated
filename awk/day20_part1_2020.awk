
function rev(s,    i, r) {
    r = ""
    for (i = length(s); i > 0; i--) r = r substr(s, i, 1)
    return r
}

function norm(s,    r) {
    r = rev(s)
    return (s < r ? s : r)
}

BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}

/^Tile/ {
    id = $2
    gsub(/[^0-9]/, "", id)
    ids[n++] = id
    row = 0
    L = ""
    R = ""
}

/^[.#]/ {
    gsub(/[ \r\t\n]+$/, "", $0)
    row++
    if (row == 1) T = $0
    if (row == 10) {
        B = $0
        L = L substr($0, 1, 1)
        R = R substr($0, length($0), 1)
        e[id, 1] = norm(T)
        e[id, 2] = norm(B)
        e[id, 3] = norm(L)
        e[id, 4] = norm(R)
    } else {
        L = L substr($0, 1, 1)
        R = R substr($0, length($0), 1)
    }
}

END {
    ans = 1
    for (i = 0; i < n; i++) {
        m = 0
        tid = ids[i]
        for (j = 1; j <= 4; j++) {
            found = 0
            edge = e[tid, j]
            for (k = 0; k < n; k++) {
                if (i == k) continue
                oid = ids[k]
                if (edge == e[oid, 1] || edge == e[oid, 2] || edge == e[oid, 3] || edge == e[oid, 4]) {
                    found = 1
                    break
                }
            }
            if (found) m++
        }
        if (m == 2) ans *= tid
    }
    printf "%.0f\n", ans
}

