
#!/bin/bash
awk '
function p(s, v, d, i, c, dp, n, l) {
    dp = 0; n = 0
    for (i = 1; i <= length(s); i++) {
        c = substr(s, i, 1)
        if (c == "[") dp++
        else if (c == "]") dp--
        else if (c ~ /[0-9]/) {
            match(substr(s, i), /^[0-9]+/)
            l = RLENGTH
            v[++n] = substr(s, i, l); d[n] = dp; i += l - 1
        }
    }
    return n
}
function e(v, d, n, i, j) {
    for (i = 1; i < n; i++) {
        if (d[i] > 4 && d[i] == d[i+1]) {
            if (i > 1) v[i-1] += v[i]
            if (i + 2 <= n) v[i+2] += v[i+1]
            v[i] = 0; d[i]--
            for (j = i + 1; j < n; j++) { v[j] = v[j+1]; d[j] = d[j+1] }
            return n - 1
        }
    }
    return 0
}
function s(v, d, n, i, j) {
    for (i = 1; i <= n; i++) {
        if (v[i] >= 10) {
            for (j = n; j >= i + 1; j--) { v[j+1] = v[j]; d[j+1] = d[j] }
            d[i]++; d[i+1] = d[i]
            v[i+1] = int((v[i] + 1) / 2); v[i] = int(v[i] / 2)
            return n + 1
        }
    }
    return 0
}
function g(v, d, n, sv, sd, t, i) {
    t = 0
    for (i = 1; i <= n; i++) {
        sv[++t] = v[i]; sd[t] = d[i]
        while (t > 1 && sd[t] == sd[t-1]) {
            sv[t-1] = 3 * sv[t-1] + 2 * sv[t]
            sd[t-1]--; t--
        }
    }
    return sv[1]
}
{
    n = p($0, v, d); pn[++cnt] = n
    for (k = 1; k <= n; k++) { vc[cnt, k] = v[k]; dc[cnt, k] = d[k] }
}
END {
    for (i = 1; i <= cnt; i++) {
        for (j = 1; j <= cnt; j++) {
            if (i == j) continue
            n = 0
            for (k = 1; k <= pn[i]; k++) { v[++n] = vc[i, k]; d[n] = dc[i, k] + 1 }
            for (k = 1; k <= pn[j]; k++) { v[++n] = vc[j, k]; d[n] = dc[j, k] + 1 }
            while (1) {
                if (r = e(v, d, n)) { n = r; continue }
                if (r = s(v, d, n)) { n = r; continue }
                break
            }
            m = g(v, d, n); if (m > max) max = m
        }
    }
    print max
}
' input.txt
