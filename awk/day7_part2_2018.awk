
BEGIN {
    A = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    while ((getline < "input.txt") > 0) {
        u = $2; v = $8
        adj[u, ++d[u]] = v
        deg[v]++
        nodes[u] = nodes[v] = 1
    }
    for (n in nodes) {
        tot++
        dur[n] = index(A, n) + 60
    }
    while (done < tot) {
        for (i = 1; i <= 5; i++) {
            if (rem[i] > 0 && --rem[i] == 0) {
                t = job[i]
                job[i] = ""
                done++
                for (j = 1; j <= d[t]; j++) deg[adj[t, j]]--
            }
        }
        for (i = 1; i <= 5; i++) {
            if (job[i] == "") {
                for (k = 1; k <= 26; k++) {
                    n = substr(A, k, 1)
                    if (nodes[n] && deg[n] <= 0 && !start[n]) {
                        job[i] = n
                        rem[i] = dur[n]
                        start[n] = 1
                        break
                    }
                }
            }
        }
        if (done == tot) break
        time++
    }
    print time
}
