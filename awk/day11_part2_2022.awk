
BEGIN {
    RS = ""; FS = "\n"
    ARGV[1] = "input.txt"; ARGC = 2
}
{
    m = NR - 1
    split($2, a, ": "); n_it[m] = split(a[2], b, ", ")
    for (i=1; i<=n_it[m]; i++) itms[m, i] = b[i]
    split($3, a, "old "); split(a[2], b, " ")
    op[m] = b[1]; v[m] = b[2]
    split($4, a, "by "); dv[m] = a[2]
    split($5, a, "monkey "); t[m] = a[2]
    split($6, a, "monkey "); f[m] = a[2]
    nm++
}
END {
    mod = 1
    for (i=0; i<nm; i++) mod *= dv[i]
    for (r=1; r<=10000; r++) {
        for (m=0; m<nm; m++) {
            cnt = n_it[m]; insp[m] += cnt
            for (i=1; i<=cnt; i++) {
                it = itms[m, i]
                val = (v[m] == "old" ? it : v[m])
                if (op[m] == "+") it += val; else it *= val
                it %= mod
                dst = (it % dv[m] == 0 ? t[m] : f[m])
                itms[dst, ++n_it[dst]] = it
            }
            n_it[m] = 0
        }
    }
    for (i=0; i<nm; i++) {
        if (insp[i] > m1) { m2 = m1; m1 = insp[i] }
        else if (insp[i] > m2) { m2 = insp[i] }
    }
    printf "%.0f\n", m1 * m2
}

