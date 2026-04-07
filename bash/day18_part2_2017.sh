
#!/bin/bash

# Reads instructions from input.txt and implements the dual-program state machine.
awk '{o[N]=$1; a[N]=$2; b[N]=$3; N++}
function v(p, k) { return k ~ /[a-z]/ ? (p ? r1[k] : r0[k]) + 0 : k + 0 }
END {
    r1["p"] = 1
    while (1) {
        f = 0
        while (i0 in o) {
            c = o[i0]; x = a[i0]; y = b[i0]
            if (c == "rcv" && h0 == t0) break
            f = 1
            if (c == "snd") q1[t1++] = v(0, x)
            else if (c == "set") r0[x] = v(0, y)
            else if (c == "add") r0[x] += v(0, y)
            else if (c == "mul") r0[x] *= v(0, y)
            else if (c == "mod") r0[x] %= v(0, y)
            else if (c == "rcv") r0[x] = q0[h0++]
            else if (c == "jgz") { if (v(0, x) > 0) i0 += v(0, y) - 1 }
            i0++
        }
        while (i1 in o) {
            c = o[i1]; x = a[i1]; y = b[i1]
            if (c == "rcv" && h1 == t1) break
            f = 1
            if (c == "snd") { q0[t0++] = v(1, x); s++ }
            else if (c == "set") r1[x] = v(1, y)
            else if (c == "add") r1[x] += v(1, y)
            else if (c == "mul") r1[x] *= v(1, y)
            else if (c == "mod") r1[x] %= v(1, y)
            else if (c == "rcv") r1[x] = q1[h1++]
            else if (c == "jgz") { if (v(1, x) > 0) i1 += v(1, y) - 1 }
            i1++
        }
        if (!f) break
    }
    print s + 0
}' input.txt

