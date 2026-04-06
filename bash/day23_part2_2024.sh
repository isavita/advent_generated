
#!/bin/bash

awk -F'-' '
{
    u=$1; v=$2
    adj[u,v] = adj[v,u] = 1
    if (!seen[u]++) list[++n] = u
    if (!seen[v]++) list[++n] = v
}
END {
    for (i = 1; i <= n; i++) {
        for (j = i + 1; j <= n; j++) {
            if (list[i] > list[j]) {
                t = list[i]; list[i] = list[j]; list[j] = t
            }
        }
    }
    solve(1, 0)
    print ans
}
function solve(idx, depth,   i, j, ok, k) {
    if (depth > max_n) {
        max_n = depth
        ans = ""
        for (k = 1; k <= depth; k++) 
            ans = (ans == "" ? "" : ans ",") list[stack[k]]
    }
    for (i = idx; i <= n; i++) {
        if (depth + n - i + 1 <= max_n) return
        ok = 1
        for (j = 1; j <= depth; j++) {
            if (!((list[stack[j]], list[i]) in adj)) {
                ok = 0; break
            }
        }
        if (ok) {
            stack[depth + 1] = i
            solve(i + 1, depth + 1)
        }
    }
}' input.txt
