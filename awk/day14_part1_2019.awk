
BEGIN {
    while ((getline < "input.txt") > 0) {
        split($0, sides, "=>")
        out = sides[2]
        sub(/^[ \t]+/, "", out)
        split(out, o, " ")
        out_amt[o[2]] = o[1]
        out_idx[o[2]] = ++out_cnt
        idx = out_cnt
        in_cnt[idx] = split(sides[1], tmp, ",")
        for (i = 1; i <= in_cnt[idx]; i++) {
            split(tmp[i], t, " ")
            in_amt[idx,i] = t[1]
            in_name[idx,i] = t[2]
        }
    }
    print need("FUEL", 1)
}

function need(chem, amt,    idx, times, ore, i) {
    if (chem == "ORE") return amt
    idx = out_idx[chem]
    if (amt <= surplus[idx]) {
        surplus[idx] -= amt
        return 0
    }
    amt -= surplus[idx]
    surplus[idx] = 0
    times = int((amt + out_amt[chem] - 1) / out_amt[chem])
    ore = 0
    for (i = 1; i <= in_cnt[idx]; i++)
        ore += need(in_name[idx,i], in_amt[idx,i] * times)
    surplus[idx] = times * out_amt[chem] - amt
    return ore
}
