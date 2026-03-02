
function solve(t, r1, r2, r3, o1, o2, o3, g,   w, wo, wc, wi, v, k) {
    if (g + t * (t - 1) / 2 <= max_g) return
    if (g > max_g) max_g = g
    if (t <= 1) return
    if (r1 > mo) r1 = mo
    if (r2 > oc) r2 = oc
    if (r3 > gi) r3 = gi
    v = t * mo - r1 * (t - 1); if (o1 > v) o1 = v
    v = t * oc - r2 * (t - 1); if (o2 > v) o2 = v
    v = t * gi - r3 * (t - 1); if (o3 > v) o3 = v
    k = t SUBSEP r1 SUBSEP r2 SUBSEP r3 SUBSEP o1 SUBSEP o2 SUBSEP o3
    if (k in memo && memo[k] >= g) return
    memo[k] = g
    wo = o1 >= go_o ? 0 : int((go_o - o1 + r1 - 1) / r1)
    wi = o3 >= go_i ? 0 : (r3 == 0 ? 99 : int((go_i - o3 + r3 - 1) / r3))
    w = (wo > wi ? wo : wi) + 1
    if (t - w > 0) solve(t - w, r1, r2, r3, o1 + r1 * w - go_o, o2 + r2 * w, o3 + r3 * w - go_i, g + t - w)
    if (r2 > 0) {
        wo = o1 >= ob_o ? 0 : int((ob_o - o1 + r1 - 1) / r1)
        wc = o2 >= ob_c ? 0 : int((ob_c - o2 + r2 - 1) / r2)
        w = (wo > wc ? wo : wc) + 1
        if (t - w > 1 && r3 < gi) solve(t - w, r1, r2, r3 + 1, o1 + r1 * w - ob_o, o2 + r2 * w - ob_c, o3 + r3 * w, g)
    }
    wo = o1 >= cl_o ? 0 : int((cl_o - o1 + r1 - 1) / r1)
    w = wo + 1
    if (t - w > 1 && r2 < oc) solve(t - w, r1, r2 + 1, r3, o1 + r1 * w - cl_o, o2 + r2 * w, o3 + r3 * w, g)
    wo = o1 >= or_o ? 0 : int((or_o - o1 + r1 - 1) / r1)
    w = wo + 1
    if (t - w > 1 && r1 < mo) solve(t - w, r1 + 1, r2, r3, o1 + r1 * w - or_o, o2 + r2 * w, o3 + r3 * w, g)
}
BEGIN {
    ans = 1
    ARGV[1] = "input.txt"
    ARGC = 2
}
NR <= 3 {
    gsub(/[^0-9]+/, " ", $0)
    $0 = $0
    or_o = $2; cl_o = $3; ob_o = $4; ob_c = $5; go_o = $6; go_i = $7
    mo = or_o; if (cl_o > mo) mo = cl_o; if (ob_o > mo) mo = ob_o; if (go_o > mo) mo = go_o
    oc = ob_c; gi = go_i; max_g = 0
    delete memo
    solve(32, 1, 0, 0, 0, 0, 0, 0)
    ans *= max_g
}
END {
    print ans
}

