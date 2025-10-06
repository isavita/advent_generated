#!/usr/bin/awk -f
BEGIN {
    n = 0
    while ((getline line < "input.txt") > 0) {
        ++n
        inst[n] = line
    }
    close("input.txt")

    for (i = 1; i <= n; i++) {
        split(inst[i], parts, " ")
        op[i] = parts[1]
        arg[i] = parts[2] + 0
    }

    for (i = 1; i <= n; i++) {
        if (op[i] == "acc") continue
        for (j = 1; j <= n; j++) mline[j] = inst[j]
        if (op[i] == "jmp") mline[i] = "nop " arg[i]
        else mline[i] = "jmp " arg[i]
        m_n = n
        acc = run()
        if (acc != -1) {
            print acc
            exit
        }
    }
    exit
}
function run(   acc, pc, visited, parts, o, a) {
    acc = 0
    for (pc = 1; pc <= m_n; pc++) visited[pc] = 0
    pc = 1
    while (pc <= m_n) {
        if (visited[pc]) return -1
        visited[pc] = 1
        split(mline[pc], parts, " ")
        o = parts[1]
        a = parts[2] + 0
        if (o == "acc") { acc += a; pc++ }
        else if (o == "jmp") { pc += a }
        else { pc++ }
    }
    return acc
}