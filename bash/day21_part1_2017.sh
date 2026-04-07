
#!/bin/bash

cat << 'EOF' > solution.awk
function split_p(p, arr,   n, r, i, j) {
    delete arr
    n = split(p, r, "/")
    for (i=1; i<=n; i++)
        for (j=1; j<=n; j++)
            arr[i-1, j-1] = substr(r[i], j, 1)
    return n
}
function join_p(arr, s,   i, j, rs, res) {
    res = ""
    for (i=0; i<s; i++) {
        rs = ""
        for (j=0; j<s; j++) rs = rs arr[i, j]
        res = res (i > 0 ? "/" : "") rs
    }
    return res
}
function rot(p,   s, t, ra, i, j) {
    s = split_p(p, t)
    for (i=0; i<s; i++)
        for (j=0; j<s; j++)
            ra[j, s-1-i] = t[i, j]
    return join_p(ra, s)
}
function flip(p,   s, t, fa, i, j) {
    s = split_p(p, t)
    for (i=0; i<s; i++)
        for (j=0; j<s; j++)
            fa[i, s-1-j] = t[i, j]
    return join_p(fa, s)
}
BEGIN {
    FS = " => "
    while ((getline < "input.txt") > 0) {
        curr = $1
        for (r=0; r<4; r++) {
            rules[curr] = $2
            rules[flip(curr)] = $2
            curr = rot(curr)
        }
    }
    N = 3
    g[0,0]="."; g[0,1]="#"; g[0,2]="."
    g[1,0]="."; g[1,1]="."; g[1,2]="#"
    g[2,0]="#"; g[2,1]="#"; g[2,2]="#"
    for (it=1; it<=5; it++) {
        k = (N % 2 == 0) ? 2 : 3
        m = k + 1
        new_N = (N / k) * m
        delete ng
        for (br=0; br<N/k; br++) {
            for (bc=0; bc<N/k; bc++) {
                sub_p = ""
                for (r=0; r<k; r++) {
                    rs = ""
                    for (c=0; c<k; c++) rs = rs g[br*k+r, bc*k+c]
                    sub_p = sub_p (r > 0 ? "/" : "") rs
                }
                out_p = rules[sub_p]
                split_p(out_p, oa)
                for (r=0; r<m; r++)
                    for (c=0; c<m; c++)
                        ng[br*m+r, bc*m+c] = oa[r, c]
            }
        }
        delete g
        for (i in ng) g[i] = ng[i]
        N = new_N
    }
    cnt = 0
    for (i in g) if (g[i] == "#") cnt++
    print cnt
}
EOF

awk -f solution.awk
rm solution.awk
