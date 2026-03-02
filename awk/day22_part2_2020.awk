function p(d1, d2,    h, c1, c2, i, n1, n2, a1, a2, s1, s2, r, w) {
    while (d1 != "" && d2 != "") {
        if (h[d1, d2]++) return "1:" d1
        c1 = d1 + 0; i = index(d1, ","); d1 = i ? substr(d1, i+1) : ""
        c2 = d2 + 0; i = index(d2, ","); d2 = i ? substr(d2, i+1) : ""
        n1 = (d1 == "" ? 0 : split(d1, a1, ","))
        n2 = (d2 == "" ? 0 : split(d2, a2, ","))
        if (n1 >= c1 && n2 >= c2) {
            s1 = a1[1]; for (i = 2; i <= c1; i++) s1 = s1 "," a1[i]
            s2 = a2[1]; for (i = 2; i <= c2; i++) s2 = s2 "," a2[i]
            r = p(s1, s2); w = substr(r, 1, 1)
        } else w = (c1 > c2 ? "1" : "2")
        if (w == "1") d1 = (d1 ? d1 "," : "") c1 "," c2
        else d2 = (d2 ? d2 "," : "") c2 "," c1
    }
    return (d1 != "" ? "1:" d1 : "2:" d2)
}
BEGIN {
    RS = ""; FS = "\n"
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    d = ""
    for (i = 2; i <= NF; i++) if ($i != "") d = (d == "" ? "" : d ",") $i
    if (NR == 1) p1 = d; else p2 = d
}
END {
    r = p(p1, p2); split(r, x, ":"); n = split(x[2], c, ",")
    for (i = 1; i <= n; i++) s += c[i] * (n - i + 1)
    print s
}