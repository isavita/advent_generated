
#!/bin/bash
awk '
{
    L[NR] = $0
    if (length($0) > W) W = length($0)
}
END {
    for (c = 1; c <= W; c++) {
        S[c] = 1
        for (r = 1; r <= NR; r++) {
            ch = substr(L[r], c, 1)
            if (ch != "" && ch != " ") { S[c] = 0; break }
        }
    }
    for (c = 1; c <= W + 1; c++) {
        if (c <= W && !S[c]) {
            if (!inB) { inB = 1; st = c }
        } else if (inB) {
            op = "+"; n = 0; split("", v)
            for (j = st; j < c; j++) {
                buf = ""
                for (i = 1; i <= NR; i++) {
                    ch = substr(L[i], j, 1)
                    if (ch ~ /[0-9]/) buf = buf ch
                    else if (ch == "+" || ch == "*") op = ch
                }
                if (buf != "") v[++n] = buf
            }
            if (n > 0) {
                res = (op == "*" ? 1 : 0)
                for (i = 1; i <= n; i++) {
                    if (op == "*") res *= v[i]
                    else res += v[i]
                }
                G += res
            }
            inB = 0
        }
    }
    printf "Grand total: %d\n", G
}' input.txt
