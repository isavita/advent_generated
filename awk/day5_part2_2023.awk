
BEGIN {
    FS = "[ :]+"
    ARGV[1] = "input.txt"
    ARGC = 2
}
/^seeds/ {
    for (i = 2; i < NF; i += 2) {
        ranges[++rc, 0] = $i
        ranges[rc, 1] = $i + $(i+1)
    }
    next
}
/map/ {
    if (mc > 0) process()
    mc = 0
    next
}
/^[0-9]/ {
    rules[++mc, 0] = $2
    rules[mc, 1] = $2 + $3
    rules[mc, 2] = $1 - $2
}
END {
    process()
    min = ranges[1, 0]
    for (i = 2; i <= rc; i++) {
        if (ranges[i, 0] < min) min = ranges[i, 0]
    }
    printf "%.0f\n", min
}
function process() {
    qc = 0
    for (i = 1; i <= rc; i++) {
        q[++qc, 0] = ranges[i, 0]
        q[qc, 1] = ranges[i, 1]
    }
    rc = 0
    while (qc > 0) {
        s = q[qc, 0]; e = q[qc, 1]; qc--
        matched = 0
        for (j = 1; j <= mc; j++) {
            ms = rules[j, 0]; me = rules[j, 1]; off = rules[j, 2]
            os = (s > ms ? s : ms)
            oe = (e < me ? e : me)
            if (os < oe) {
                ranges[++rc, 0] = os + off
                ranges[rc, 1] = oe + off
                if (s < os) { q[++qc, 0] = s; q[qc, 1] = os }
                if (e > oe) { q[++qc, 0] = oe; q[qc, 1] = e }
                matched = 1
                break
            }
        }
        if (!matched) {
            ranges[++rc, 0] = s
            ranges[rc, 1] = e
        }
    }
}
