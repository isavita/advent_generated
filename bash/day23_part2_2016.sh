
#!/bin/bash
awk '
function get(v) { return (v ~ /^[a-d]$/) ? r[v] : v+0 }
BEGIN {
    while ((getline < "input.txt") > 0) instr[n++] = $0
    r["a"] = 12; r["b"] = 0; r["c"] = 0; r["d"] = 0
    for (i = 0; i < n; ) {
        if (i + 5 < n) {
            split(instr[i], p0); split(instr[i+1], p1); split(instr[i+2], p2)
            split(instr[i+3], p3); split(instr[i+4], p4); split(instr[i+5], p5)
            if (p0[1]=="cpy" && p1[1]=="inc" && p2[1]=="dec" && p3[1]=="jnz" && p4[1]=="dec" && p5[1]=="jnz") {
                if (p1[2]=="a" && p2[2]==p0[3] && p3[2]==p0[3] && p3[3]=="-2" && p4[2]=="d" && p5[2]=="d" && p5[3]=="-5") {
                    r["a"] += get(p0[2]) * r["d"]
                    r[p0[3]] = 0; r["d"] = 0
                    i += 6; continue
                }
            }
        }
        split(instr[i], p)
        if (p[1] == "cpy") { if (p[3] ~ /^[a-d]$/) r[p[3]] = get(p[2]); i++ }
        else if (p[1] == "inc") { if (p[2] ~ /^[a-d]$/) r[p[2]]++; i++ }
        else if (p[1] == "dec") { if (p[2] ~ /^[a-d]$/) r[p[2]]--; i++ }
        else if (p[1] == "jnz") { i += (get(p[2]) != 0 ? get(p[3]) : 1) }
        else if (p[1] == "tgl") {
            t = i + get(p[2])
            if (t >= 0 && t < n) {
                nf = split(instr[t], tp)
                if (nf == 2) instr[t] = (tp[1] == "inc" ? "dec" : "inc") " " tp[2]
                else instr[t] = (tp[1] == "jnz" ? "cpy" : "jnz") " " tp[2] " " tp[3]
            }
            i++
        } else i++
    }
    print r["a"]
}'
