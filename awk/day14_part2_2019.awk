
BEGIN {
    id["ORE"] = 0; names[0] = "ORE"; N = 1
    while ((getline < "input.txt") > 0) {
        n = split($0, p, "=>")
        split(p[2], q, " "); outAmt = q[1]; outName = q[2]
        if (!(outName in id)) { id[outName] = N; names[N] = outName; ++N }
        oid = id[outName]
        prod[oid] = outAmt
        split(p[1], r, ",")
        for (i = 1; i <= length(r); i++) {
            split(r[i], s, " ")
            inAmt = s[1]; inName = s[2]
            if (!(inName in id)) { id[inName] = N; names[N] = inName; ++N }
            inp[oid, ++cnt[oid]] = id[inName]
            need[oid, cnt[oid]] = inAmt
        }
    }
    fuel = id["FUEL"]
    avail = 1000000000000
    lo = 0; hi = avail
    while (lo <= hi) {
        mid = int((lo + hi)/2)
        delete surp
        ore = 0
        calc(fuel, mid)
        if (ore <= avail) { best = mid; lo = mid + 1 }
        else { hi = mid - 1 }
    }
    print best
}
function calc(c, amt,    i, t, u, k) {
    if (c == 0) { ore += amt; return }
    if (surp[c] >= amt) { surp[c] -= amt; return }
    amt -= surp[c]; surp[c] = 0
    t = int((amt + prod[c] - 1)/prod[c])
    for (i = 1; i <= cnt[c]; i++) {
        u = inp[c,i]; k = need[c,i]*t
        calc(u, k)
    }
    surp[c] += t*prod[c] - amt
}
