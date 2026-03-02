
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
    FS = "[{}]"
}
$1 != "" && NF > 1 {
    flow[$1] = $2
    next
}
NF > 1 {
    split($2, p, ",")
    for (j = 1; j <= 4; j++) {
        split(p[j], kv, "=")
        v[kv[1]] = kv[2]
    }
    if (chk("in")) t += v["x"] + v["m"] + v["a"] + v["s"]
}
function chk(w,    rs, n, i, r, c, k, op, lim) {
    if (w == "A") return 1
    if (w == "R") return 0
    n = split(flow[w], rs, ",")
    for (i = 1; i <= n; i++) {
        r = rs[i]
        if (index(r, ":")) {
            split(r, c, ":")
            k = substr(c[1], 1, 1)
            op = substr(c[1], 2, 1)
            lim = substr(c[1], 3) + 0
            if ((op == ">" && v[k] > lim) || (op == "<" && v[k] < lim)) return chk(c[2])
        } else return chk(r)
    }
}
END {
    print t + 0
}
