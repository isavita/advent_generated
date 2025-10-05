#!/usr/bin/awk -f
BEGIN {
    file = "input.txt"
    while ((getline line < file) > 0) {
        if (line ~ /^initial state/) {
            idx = index(line, ":")
            initial = ""
            if (idx > 0) {
                initial = substr(line, idx+1)
                gsub(/^[ \t]+|[ \t]+$/, "", initial)
            }
            len = length(initial)
            for (i = 1; i <= len; i++) {
                c = substr(initial, i, 1)
                if (c == "#") {
                    pos = i - 1
                    state[pos] = 1
                }
            }
        } else if (line ~ /=>/) {
            split(line, parts, "=>")
            left = parts[1]
            right = parts[2]
            gsub(/^[ \t]+|[ \t]+$/, "", left)
            gsub(/^[ \t]+|[ \t]+$/, "", right)
            rules[left] = right
        }
    }

    for (gen = 1; gen <= 20; gen++) {
        min = ""
        max = ""
        for (p in state) {
            if (state[p] == 1) {
                if (min == "" || (p + 0) < (min + 0)) min = p
                if (max == "" || (p + 0) > (max + 0)) max = p
            }
        }
        if (min == "" && max == "") { break }

        delete nextstate
        minN = min + 0
        maxN = max + 0
        for (i = minN - 2; i <= maxN + 2; i++) {
            pattern = ""
            for (k = i - 2; k <= i + 2; k++) {
                if ((k in state) && state[k] == 1) {
                    pattern = pattern "#"
                } else {
                    pattern = pattern "."
                }
            }
            if (pattern in rules) {
                if (rules[pattern] == "#") {
                    nextstate[i] = 1
                }
            }
        }
        delete state
        for (p in nextstate) state[p] = nextstate[p]
    }

    sum = 0
    for (p in state) sum += (p + 0)
    print sum
    exit
}