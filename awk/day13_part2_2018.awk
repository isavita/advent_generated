BEGIN {
    FS = ""
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    for (x = 1; x <= NF; x++) {
        c = $x
        if (c ~ /[<>^v]/) {
            i = ++n; cX[i] = x - 1; cY[i] = NR - 1; cT[i] = 0; cA[i] = 1
            cD[i] = index("^>v<", c) - 1
            c = (c ~ /[<>]/ ? "-" : "|")
        }
        m[NR - 1, x - 1] = c
    }
}
END {
    ac = n
    while (ac > 1) {
        o_n = 0
        for (i = 1; i <= n; i++) if (cA[i]) ord[++o_n] = i
        for (i = 1; i < o_n; i++) {
            for (j = i + 1; j <= o_n; j++) {
                p = ord[i]; q = ord[j]
                if (cY[p] > cY[q] || (cY[p] == cY[q] && cX[p] > cX[q])) {
                    ord[i] = q; ord[j] = p
                }
            }
        }
        for (k = 1; k <= o_n; k++) {
            i = ord[k]
            if (!cA[i]) continue
            if (cD[i] == 0) cY[i]--; else if (cD[i] == 1) cX[i]++
            else if (cD[i] == 2) cY[i]++; else cX[i]--
            for (j = 1; j <= n; j++) {
                if (i != j && cA[j] && cX[i] == cX[j] && cY[i] == cY[j]) {
                    cA[i] = 0; cA[j] = 0; ac -= 2; break
                }
            }
            if (!cA[i]) continue
            t = m[cY[i], cX[i]]
            if (t == "+") {
                if (cT[i] == 0) cD[i] = (cD[i] + 3) % 4
                else if (cT[i] == 2) cD[i] = (cD[i] + 1) % 4
                cT[i] = (cT[i] + 1) % 3
            } else if (t == "/") cD[i] = (5 - cD[i]) % 4
            else if (t == "\\") cD[i] = 3 - cD[i]
        }
    }
    for (i = 1; i <= n; i++) if (cA[i]) print cX[i] "," cY[i]
}