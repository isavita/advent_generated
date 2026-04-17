
#!/usr/bin/env bash
set -euo pipefail

awk '
function val(x) { return (x ~ /^-?[0-9]+$/ ? x : (r[x] + 0)) }

{
    op[NR] = $1
    x[NR]  = $2
    y[NR]  = $3
}

END {
    ip = 1
    last = 0

    while (ip >= 1 && ip <= NR) {
        ins = op[ip]

        if (ins == "snd") {
            last = val(x[ip])
            ip++
        } else if (ins == "set") {
            r[x[ip]] = val(y[ip])
            ip++
        } else if (ins == "add") {
            r[x[ip]] += val(y[ip])
            ip++
        } else if (ins == "mul") {
            r[x[ip]] *= val(y[ip])
            ip++
        } else if (ins == "mod") {
            r[x[ip]] %= val(y[ip])
            ip++
        } else if (ins == "rcv") {
            if (val(x[ip]) != 0) {
                print last
                exit
            }
            ip++
        } else if (ins == "jgz") {
            if (val(x[ip]) > 0) ip += val(y[ip])
            else ip++
        } else {
            ip++
        }
    }
}
' input.txt
