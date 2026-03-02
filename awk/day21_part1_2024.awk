function rep(c, n, r) { r=""; while(n-->0) r=r c; return r }
function pos(ch, n, p) {
    p = n ? index("789456123 0A", ch)-1 : index(" ^A<v>", ch)-1
    tr = int(p/3); tc = p%3
}
function ok(r, c, s, n, k, m) {
    for (k=1; k<=length(s); k++) {
        m=substr(s, k, 1)
        if (m=="^") r--; else if (m=="v") r++; else if (m=="<") c--; else if (m==">") c++
        if (r<0 || c<0 || c>2 || (n && r>3) || (!n && r>1) || (n && r==3 && c==0) || (!n && r==0 && c==0)) return 0
    }
    return 1
}
function mov(r1, c1, r2, c2, n, m) {
    m = (c1>c2?rep("<",c1-c2):"") (r1>r2?rep("^",r1-r2):"") (r1<r2?rep("v",r2-r1):"") (c1<c2?rep(">",c2-c1):"")
    if (ok(r1, c1, m, n)) return m
    return (c1<c2?rep(">",c2-c1):"") (r1>r2?rep("^",r1-r2):"") (r1<r2?rep("v",r2-r1):"") (c1>c2?rep("<",c1-c2):"")
}
function solve(cd, rb, tot, cr, cc, j, ch, mv, trs, tcs) {
    if (rb <= 0) return length(cd)
    if (memo[cd, rb]) return memo[cd, rb]
    cr = (rb == maxR ? 3 : 0); cc = 2
    for (j=1; j<=length(cd); j++) {
        ch = substr(cd, j, 1); pos(ch, rb == maxR); trs = tr; tcs = tc
        mv = mov(cr, cc, trs, tcs, rb == maxR)
        tot += solve(mv "A", rb - 1)
        cr = trs; cc = tcs
    }
    return memo[cd, rb] = tot
}
BEGIN {
    maxR = 3
    while ((getline < "input.txt") > 0) {
        c = $0; gsub(/[ \r\n\t]/, "", c); if (c == "") continue
        n = c; gsub(/[^0-9]/, "", n)
        ans += solve(c, maxR) * int(n)
    }
    print ans
}