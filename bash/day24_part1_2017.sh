
#!/bin/bash
awk -F/ '
function d(c, s, i, v) {
    if (s > m) m = s
    for (i = 1; i <= n; i++) {
        if (!u[i] && (a[i] == c || b[i] == c)) {
            u[i] = 1
            v = (a[i] == c ? b[i] : a[i])
            d(v, s + a[i] + b[i])
            u[i] = 0
        }
    }
}
{
    a[++n] = $1
    b[n] = $2
}
END {
    d(0, 0)
    print m
}' input.txt
