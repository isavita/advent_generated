
#!/bin/bash

awk '
function beam(x, y,    m, i, r, c, op, ms, m1, m2, m3, v1, v2, ta, in_q, in_p, out) {
    for (i in mem_init) m[i] = mem_init[i]
    i = r = in_p = out = 0
    in_q[0] = x; in_q[1] = y
    while (m[i] != 99) {
        op = m[i] % 100; ms = int(m[i] / 100)
        m1 = ms % 10; m2 = int(ms / 10) % 10; m3 = int(ms / 100) % 10
        f = i; 
        if (op == 1 || op == 2 || op == 7 || op == 8) {
            v1 = (m1==0?m[m[i+1]]:(m1==1?m[i+1]:m[r+m[i+1]]))+0
            v2 = (m2==0?m[m[i+2]]:(m2==1?m[i+2]:m[r+m[i+2]]))+0
            ta = (m3==0?m[i+3]:r+m[i+3])+0
            if (op == 1) m[ta] = v1 + v2
            if (op == 2) m[ta] = v1 * v2
            if (op == 7) m[ta] = (v1 < v2 ? 1 : 0)
            if (op == 8) m[ta] = (v1 == v2 ? 1 : 0)
            i += 4
        } else if (op == 3) {
            ta = (m1==0?m[i+1]:r+m[i+1])+0
            m[ta] = in_q[in_p++]
            i += 2
        } else if (op == 4) {
            out = (m1==0?m[m[i+1]]:(m1==1?m[i+1]:m[r+m[i+1]]))+0
            i += 2
        } else if (op == 5 || op == 6) {
            v1 = (m1==0?m[m[i+1]]:(m1==1?m[i+1]:m[r+m[i+1]]))+0
            v2 = (m2==0?m[m[i+2]]:(m2==1?m[i+2]:m[r+m[i+2]]))+0
            if ((op == 5 && v1 != 0) || (op == 6 && v1 == 0)) i = v2; else i += 3
        } else if (op == 9) {
            r += (m1==0?m[m[i+1]]:(m1==1?m[i+1]:m[r+m[i+1]]))+0
            i += 2
        }
    }
    return out
}
BEGIN {
    RS = ","
    while ((getline val < "input.txt") > 0) mem_init[n++] = val
    y = 100; x = 0
    while (1) {
        while (!beam(x, y)) x++
        if (beam(x + 99, y - 99)) {
            print x * 10000 + (y - 99)
            exit
        }
        y++
    }
}'
