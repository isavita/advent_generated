BEGIN {
    while ((getline < "input.txt") > 0) {
        if ($0 == "") { s = 1; continue }
        if (!s) { split($0, a, ": "); R[a[1]] = a[2] }
        else MS[++mc] = $0
    }
    s42 = res(42); split(s42, a, "|"); for (i in a) A[a[i]]
    s31 = res(31); split(s31, b, "|"); for (i in b) B[b[i]]
    for (i in A) { L = length(i); break }
    for (i = 1; i <= mc; i++) {
        msg = MS[i]; len = length(msg)
        if (len % L == 0) {
            n = len / L; k = 1; c42 = 0; c31 = 0
            while (k <= n) {
                chunk = substr(msg, (k-1)*L + 1, L)
                if (chunk in A) { c42++; k++ } else break
            }
            while (k <= n) {
                chunk = substr(msg, (k-1)*L + 1, L)
                if (chunk in B) { c31++; k++ } else break
            }
            if (k > n && c31 >= 1 && c42 > c31) ans++
        }
    }
    print ans
}
function res(id, i,j,k,r,m,o,p,no,ors,nc,sr,sa,ns,t,rs,v,c) {
    if (id in M) return M[id]
    if (R[id] ~ /"/) {
        v = R[id]; gsub(/"| /, "", v); return M[id] = v
    }
    no = split(R[id], o, "|")
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
}