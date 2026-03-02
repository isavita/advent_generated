
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    if (split($0, a, " \\(contains ") < 2) next
    gsub(/[(),]/, "", a[2])
    ni = split(a[1], il)
    na = split(a[2], al)
    split("", cr)
    for (i = 1; i <= ni; i++) {
        c[il[i]]++
        cr[il[i]] = 1
    }
    for (i = 1; i <= na; i++) {
        an = al[i]
        if (!(an in sn)) {
            sn[an] = 1
            for (j in cr) p[an, j] = 1
        } else {
            for (k in p) {
                split(k, pt, SUBSEP)
                if (pt[1] == an && !(pt[2] in cr)) {
                    delete p[k]
                }
            }
        }
    }
}
END {
    for (k in p) {
        split(k, pt, SUBSEP)
        u[pt[2]] = 1
    }
    for (i in c) {
        if (!(i in u)) s += c[i]
    }
    print s + 0
}
