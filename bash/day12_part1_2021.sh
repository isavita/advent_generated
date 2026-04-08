
awk -F- '{
    adj[$1, ++n[$1]] = $2
    adj[$2, ++n[$2]] = $1
}
function walk(u,    i, v, res) {
    if (u == "end") return 1
    if (u ~ /^[a-z]+$/) vis[u] = 1
    for (i = 1; i <= n[u]; i++) {
        v = adj[u, i]
        if (!vis[v]) res += walk(v)
    }
    vis[u] = 0
    return res + 0
}
END {
    print walk("start")
}' input.txt
