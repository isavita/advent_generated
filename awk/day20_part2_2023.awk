
#!/usr/bin/awk -f
BEGIN {
    FS = " -> "
    while ((getline < "input.txt") > 0) {
        if ($1 == "broadcaster") {
            name = "broadcaster"; type[name] = "br"
        } else {
            type[substr($1, 2)] = substr($1, 1, 1); name = substr($1, 2)
        }
        n = split($2, d, ", ")
        for (i = 1; i <= n; i++) {
            dests[name, ++dests_count[name]] = d[i]
            inputs[d[i], ++inputs_count[d[i]]] = name
        }
    }
    for (m in dests_count) {
        for (i = 1; i <= dests_count[m]; i++)
            if (dests[m, i] == "rx") target = m
    }
    for (i = 1; i <= inputs_count[target]; i++) {
        f = inputs[target, i]; feeders[f] = 1; num_feeders++
    }
    presses = 0; found = 0
    while (found < num_feeders) {
        presses++
        head = 1; tail = 1
        q_to[tail] = "broadcaster"; q_p[tail] = 0; q_from[tail] = "button"; tail++
        while (head < tail) {
            u = q_to[head]; p = q_p[head]; fr = q_from[head]; head++
            if (u == target && p == 1 && feeders[fr] && !cycle[fr]) {
                cycle[fr] = presses; found++
            }
            if (type[u] == "br") {
                for (i = 1; i <= dests_count[u]; i++) {
                    q_to[tail] = dests[u, i]; q_p[tail] = p; q_from[tail] = u; tail++
                }
            } else if (type[u] == "%") {
                if (p == 0) {
                    ff[u] = (ff[u] == 0 ? 1 : 0)
                    for (i = 1; i <= dests_count[u]; i++) {
                        q_to[tail] = dests[u, i]; q_p[tail] = ff[u]; q_from[tail] = u; tail++
                    }
                }
            } else if (type[u] == "&") {
                mem[u, fr] = p
                all_h = 1
                for (i = 1; i <= inputs_count[u]; i++)
                    if (mem[u, inputs[u, i]] == 0) { all_h = 0; break }
                out_p = (all_h == 1 ? 0 : 1)
                for (i = 1; i <= dests_count[u]; i++) {
                    q_to[tail] = dests[u, i]; q_p[tail] = out_p; q_from[tail] = u; tail++
                }
            }
        }
    }
    ans = 1
    for (f in cycle) ans *= cycle[f]
    printf "%.0f\n", ans
}

