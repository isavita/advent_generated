
#!/bin/bash
awk '
{
    for (i = 1; i <= length($0); i++) {
        c = substr($0, i, 1)
        if (c ~ /[<>^v]/) {
            x[n] = i - 1; y[n] = NR - 1; d[n] = c; t[n] = 0; n++
            c = (c == ">" || c == "<") ? "-" : "|"
        }
        g[i - 1, NR - 1] = c
    }
}
END {
    while (1) {
        for (i = 0; i < n; i++) o[i] = i
        for (i = 0; i < n; i++) {
            for (j = i + 1; j < n; j++) {
                if (y[o[i]] > y[o[j]] || (y[o[i]] == y[o[j]] && x[o[i]] > x[o[j]])) {
                    tmp = o[i]; o[i] = o[j]; o[j] = tmp
                }
            }
        }
        for (i = 0; i < n; i++) {
            idx = o[i]
            if (d[idx] == "^") y[idx]--
            else if (d[idx] == "v") y[idx]++
            else if (d[idx] == "<") x[idx]--
            else if (d[idx] == ">") x[idx]++
            for (j = 0; j < n; j++) {
                if (idx != j && x[idx] == x[j] && y[idx] == y[j]) {
                    print x[idx] "," y[idx]; exit
                }
            }
            cur = g[x[idx], y[idx]]
            if (cur == "/") {
                if (d[idx] == "^") d[idx] = ">"; else if (d[idx] == "v") d[idx] = "<"
                else if (d[idx] == "<") d[idx] = "v"; else if (d[idx] == ">") d[idx] = "^"
            } else if (cur == "\\") {
                if (d[idx] == "^") d[idx] = "<"; else if (d[idx] == "v") d[idx] = ">"
                else if (d[idx] == "<") d[idx] = "^"; else if (d[idx] == ">") d[idx] = "v"
            } else if (cur == "+") {
                if (t[idx] == 0) {
                    if (d[idx] == "^") d[idx] = "<"; else if (d[idx] == "v") d[idx] = ">"
                    else if (d[idx] == "<") d[idx] = "v"; else if (d[idx] == ">") d[idx] = "^"
                } else if (t[idx] == 2) {
                    if (d[idx] == "^") d[idx] = ">"; else if (d[idx] == "v") d[idx] = "<"
                    else if (d[idx] == "<") d[idx] = "^"; else if (d[idx] == ">") d[idx] = "v"
                }
                t[idx] = (t[idx] + 1) % 3
            }
        }
    }
}' input.txt
