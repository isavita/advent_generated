
BEGIN {
    while ((getline line < "input.txt") > 0) {
        n = split(line, parts, /[ \t\r\n]+/)
        for (i = 1; i <= n; i++) a[++cnt] = parts[i] + 0
    }
    close("input.txt")
    pos = 1
    print parse()
}
function parse(    childCount, metaCount, i, v, m, childVals) {
    childCount = a[pos++]
    metaCount  = a[pos++]
    delete childVals
    for (i = 1; i <= childCount; i++) childVals[i] = parse()
    if (childCount == 0) {
        v = 0
        for (i = 1; i <= metaCount; i++) v += a[pos++]
    } else {
        v = 0
        for (i = 1; i <= metaCount; i++) {
            m = a[pos++]
            if (m >= 1 && m <= childCount) v += childVals[m]
        }
    }
    return v
}
