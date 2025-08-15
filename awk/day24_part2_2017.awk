
#!/usr/bin/awk -f

function build(port, len, str,   i, nxt) {
    if (len > maxLen || (len == maxLen && str > maxStr)) {
        maxLen = len
        maxStr = str
    }
    for (i = 1; i <= n; i++) {
        if (!used[i] && (p1[i] == port || p2[i] == port)) {
            used[i] = 1
            nxt = (p1[i] == port) ? p2[i] : p1[i]
            build(nxt, len+1, str + p1[i] + p2[i])
            used[i] = 0
        }
    }
}

BEGIN {
    n = 0
    while ((getline line < "input.txt") > 0) {
        split(line, a, "/")
        ++n
        p1[n] = a[1] + 0
        p2[n] = a[2] + 0
        used[n] = 0
    }
    close("input.txt")
    maxLen = 0
    maxStr = 0
    build(0,0,0)
    print maxStr
}
