
#!/bin/bash

awk '
function gcd(a, b) {
    while (b) {
        tmp = a % b
        a = b
        b = tmp
    }
    return a
}
function lcm(a, b) {
    return (a / gcd(a, b)) * b
}
NR == 1 { 
    instr = $0
    ilen = length($0) 
}
NR > 2 {
    node = $1
    L = substr($3, 2, 3)
    R = substr($4, 1, 3)
    map[node "L"] = L
    map[node "R"] = R
    if (node ~ /A$/) starts[++sc] = node
}
END {
    res = 0
    for (i = 1; i <= sc; i++) {
        curr = starts[i]
        steps = 0
        while (curr !~ /Z$/) {
            dir = substr(instr, (steps % ilen) + 1, 1)
            curr = map[curr dir]
            steps++
        }
        res = (res == 0) ? steps : lcm(res, steps)
    }
    printf "%.0f\n", res
}' input.txt
