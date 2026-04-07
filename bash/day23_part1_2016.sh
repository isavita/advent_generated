
#!/bin/bash

awk '
function v(x) { return x ~ /^[a-d]$/ ? r[x] : x + 0 }
{ i[n++] = $0 }
END {
    r["a"] = 7
    p = 0
    while (p in i) {
        m = split(i[p], f)
        o = f[1]; a = f[2]; b = f[3]
        if (o == "cpy") {
            if (b ~ /^[a-d]$/) r[b] = v(a)
            p++
        } else if (o == "inc") {
            if (a ~ /^[a-d]$/) r[a]++
            p++
        } else if (o == "dec") {
            if (a ~ /^[a-d]$/) r[a]--
            p++
        } else if (o == "jnz") {
            p += v(a) ? v(b) : 1
        } else if (o == "tgl") {
            t = p + v(a)
            if (t in i) {
                c = split(i[t], g)
                i[t] = (c == 3 ? (g[1] == "jnz" ? "cpy" : "jnz") : (g[1] == "inc" ? "dec" : "inc")) " " g[2] (c == 3 ? " " g[3] : "")
            }
            p++
        } else {
            p++
        }
    }
    print r["a"]
}' input.txt
