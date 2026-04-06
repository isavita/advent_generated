
#!/bin/bash
awk '
{
    n = split($0, chars, "")
    x = 0; y = 0; d = 0; sp = 0
    m[0, 0] = 0
    for (i = 1; i <= n; i++) {
        c = chars[i]
        if (c == "(") {
            stack[++sp] = x " " y " " d
        } else if (c == "|") {
            split(stack[sp], state)
            x = state[1]; y = state[2]; d = state[3]
        } else if (c == ")") {
            split(stack[sp], state)
            x = state[1]; y = state[2]; d = state[3]; sp--
        } else if (c ~ /[NSEW]/) {
            if (c == "N") y--; else if (c == "S") y++
            else if (c == "E") x++; else if (c == "W") x--
            d++
            if (!((x, y) in m) || d < m[x, y]) m[x, y] = d
        }
    }
}
END {
    for (key in m) {
        if (m[key] > max_d) max_d = m[key]
        if (m[key] >= 1000) count_1000++
    }
    print max_d
    print count_1000
}' input.txt
