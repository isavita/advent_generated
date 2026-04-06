
#!/bin/bash
awk '
function mul2024(s,   len, i, c, v, out) {
    len = length(s)
    c = 0; out = ""
    for (i = len; i > 0; i--) {
        v = substr(s, i, 1) * 2024 + c
        out = (v % 10) out
        c = int(v / 10)
    }
    return (c > 0 ? c : "") out
}
{
    for (i = 1; i <= NF; i++) count[$i]++
}
END {
    for (step = 1; step <= 75; step++) {
        split("", nxt)
        for (s in count) {
            n = count[s]
            len = length(s)
            if (s == "0") {
                nxt["1"] += n
            } else if (len % 2 == 0) {
                l = substr(s, 1, len/2) + 0
                r = substr(s, len/2 + 1) + 0
                nxt[l] += n
                nxt[r] += n
            } else {
                if (len < 12) {
                    nxt[s * 2024] += n
                } else {
                    nxt[mul2024(s)] += n
                }
            }
        }
        delete count
        for (s in nxt) count[s] = nxt[s]
    }
    total = 0
    for (s in count) total += count[s]
    printf "%.0f\n", total
}' input.txt
