BEGIN {
    FS = " -> |, "
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    n = $1; m = (n ~ /^[%&]/) ? substr(n, 2) : n
    type[m] = (n ~ /^[%&]/) ? substr(n, 1, 1) : "b"
    for (i = 2; i <= NF; i++) {
        if ($i != "") {
            dests[m, ++num_dests[m]] = $i
            srcs[$i] = srcs[$i] m " "
        }
    }
}
END {
    for (m in type) {
        if (type[m] == "&") {
            ic[m] = split(srcs[m], s_arr, " ")
            for (i = 1; i <= ic[m]; i++) conj_mem[m, s_arr[i]] = 0
        }
    }
    for (p = 1; p <= 1000; p++) {
        h = 1; t = 1; qs[t] = "button"; qd[t] = "broadcaster"; qv[t] = 0; t++
        while (h < t) {
            s = qs[h]; d = qd[h]; v = qv[h]; h++
            if (v == 0) lo++; else hi++
            if (!(d in type)) continue
            sd = -1
            if (type[d] == "b") sd = v
            else if (type[d] == "%") {
                if (v == 0) { st[d] = 1 - st[d]; sd = st[d] }
            } else if (type[d] == "&") {
                if (v != conj_mem[d, s]) {
                    if (v == 1) hc[d]++; else hc[d]--
                    conj_mem[d, s] = v
                }
                sd = (hc[d] == ic[m=d]) ? 0 : 1
            }
            if (sd != -1) {
                for (i = 1; i <= num_dests[d]; i++) {
                    qs[t] = d; qd[t] = dests[d, i]; qv[t] = sd; t++
                }
            }
        }
    }
    printf "%.0f\n", lo * hi
}