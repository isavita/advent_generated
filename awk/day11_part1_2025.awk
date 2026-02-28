
BEGIN{
    while ((getline line < "input.txt") > 0) {
        if (line ~ /^[[:space:]]*$/) continue
        split(line, p, ":")
        node = p[1]; gsub(/^[[:space:]]+|[[:space:]]+$/, "", node)
        if (length(p) > 1) {
            neigh = p[2]; gsub(/^[[:space:]]+|[[:space:]]+$/, "", neigh)
            adj[node] = neigh
        } else adj[node] = ""
    }
    close("input.txt")
    print dfs("you")
}
function dfs(u,  v, cnt, n, a, i){
    if (u == "out") return 1
    if (u in memo) return memo[u]
    cnt = 0
    n = split(adj[u], a, " ")
    for (i = 1; i <= n; i++) {
        v = a[i]
        if (v != "") cnt += dfs(v)
    }
    memo[u] = cnt
    return cnt
}
