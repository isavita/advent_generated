
#!/usr/bin/awk -f
function swap(a, i, j, t) { t = a[i]; a[i] = a[j]; a[j] = t }
function run_amp(id,   instr, op, m1, m2, p1, p2, p3, v1, v2) {
    while (1) {
        instr = mem[id, pc[id]]; op = instr % 100
        m1 = int(instr / 100) % 10; m2 = int(instr / 1000) % 10
        if (op == 99) { halted[id] = 1; return "HALT" }
        p1 = mem[id, pc[id]+1]; p2 = mem[id, pc[id]+2]; p3 = mem[id, pc[id]+3]
        v1 = (m1 == 1 ? p1 : mem[id, p1]); v2 = (m2 == 1 ? p2 : mem[id, p2])
        if (op == 1) { mem[id, p3] = v1 + v2; pc[id] += 4 }
        else if (op == 2) { mem[id, p3] = v1 * v2; pc[id] += 4 }
        else if (op == 3) {
            if (in_ptr[id] > in_count[id]) return "WAIT"
            mem[id, p1] = in_queue[id, in_ptr[id]++]; pc[id] += 2
        }
        else if (op == 4) { pc[id] += 2; last_out = v1; return "OUT" }
        else if (op == 5) { pc[id] = (v1 != 0 ? v2 : pc[id] + 3) }
        else if (op == 6) { pc[id] = (v1 == 0 ? v2 : pc[id] + 3) }
        else if (op == 7) { mem[id, p3] = (v1 < v2 ? 1 : 0); pc[id] += 4 }
        else if (op == 8) { mem[id, p3] = (v1 == v2 ? 1 : 0); pc[id] += 4 }
    }
}
function solve(phases,   ps, i, addr, signal, current, last_ts, res) {
    split(phases, ps, " ")
    for (i=0; i<5; i++) {
        pc[i] = 0; halted[i] = 0; in_ptr[i] = 1; in_count[i] = 0
        for (addr=0; addr<n_prog; addr++) mem[i, addr] = prog[addr]
        in_queue[i, ++in_count[i]] = ps[i+1]
    }
    signal = 0; current = 0; last_ts = 0
    while (!halted[4]) {
        in_queue[current, ++in_count[current]] = signal
        while (1) {
            res = run_amp(current)
            if (res == "OUT") {
                signal = last_out
                if (current == 4) last_ts = signal
                break
            }
            if (res == "HALT" || res == "WAIT") break
        }
        current = (current + 1) % 5
    }
    return last_ts
}
function perm(k, a,   i, val) {
    if (k == 5) {
        val = solve(a[1] " " a[2] " " a[3] " " a[4] " " a[5])
        if (val > max) max = val
    } else {
        for (i=k; i<=5; i++) {
            swap(a, k, i); perm(k+1, a); swap(a, k, i)
        }
    }
}
BEGIN {
    FS = ","
    while ((getline < "input.txt") > 0) {
        for (i=1; i<=NF; i++) prog[n_prog++] = $i
    }
    a[1]=0; a[2]=1; a[3]=2; a[4]=3; a[5]=4; max=0; perm(1, a)
    print "Part One: The highest signal that can be sent to the thrusters is " max "."
    a[1]=5; a[2]=6; a[3]=7; a[4]=8; a[5]=9; max=0; perm(1, a)
    print "Part Two: The highest signal that can be sent to the thrusters with feedback loop is " max "."
}
