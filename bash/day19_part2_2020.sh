
awk '
!s {
    if ($0 == "") { s = 1; next }
    split($0, a, ": "); R[a[1]] = a[2]; next
}
{ MS[++mc] = $0 }
END {
    s42 = res(42); split(s42, a, "|"); for (i in a) A[a[i]]
    s31 = res(31); split(s31, b, "|"); for (i in b) B[b[i]]
    for (i in A) { L = length(i); break }
    for (i = 1; i <= mc; i++) {
        msg = MS[i]; len = length(msg); n = len / L
        if (len % L == 0) {
            k = 1; c42 = 0; c31 = 0
            while (k <= n && (substr(msg, (k-1)*L + 1, L) in A)) { c42++; k++ }
            while (k <= n && (substr(msg, (k-1)*L + 1, L) in B)) { c31++; k++ }
            if (k > n && c31 >= 1 && c42 > c31) ans++
        }
    }
    print ans + 0
}
function res(id, i, j, k, r, m, o, p, no, ors, nc, sr, sa, ns, t, rs, v, c) {
    if (id in M) return M[id]
    if (R[id] ~ /"/) {
        v = R[id]; gsub(/"| /, "", v); return M[id] = v
    }
    no = split(R[id], o, "|"); rs = ""
    for (i = 1; i <= no; i++) {
        nc = split(o[i], p, " ")
        delete ors; ors[1] = ""; m = 1
        for (j = 1; j <= nc; j++) {
            sr = res(p[j]); ns = split(sr, sa, "|")
            delete t; c = 1
            for (k = 1; k <= m; k++)
                for (r = 1; r <= ns; r++)
                    t[c++] = ors[k] sa[r]
            m = c - 1; delete ors; for (k = 1; k <= m; k++) ors[k] = t[k]
        }
        for (k = 1; k <= m; k++) rs = (rs == "" ? "" : rs "|") ors[k]
    }
    return M[id] = rs
}' input.txt
