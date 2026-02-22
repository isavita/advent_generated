
BEGIN{
    while ((getline line < "input.txt") > 0) {
        split(line, a, " ")
        p1 = a[1]
        sign = (a[3] == "gain") ? 1 : -1
        val = sign * a[4]
        p2 = a[11]
        sub(/\./, "", p2)
        h[p1 SUBSEP p2] = val
        people[p1]; people[p2]
    }
    n = 0
    for (p in people) list[++n] = p
    max = -1e9
    dfs(0)
    print max
}
function dfs(depth,   i, p){
    if (depth == n) {
        total = 0
        for (i = 1; i <= n; i++) {
            p1 = order[i]
            p2 = order[(i % n) + 1]
            total += h[p1 SUBSEP p2] + h[p2 SUBSEP p1]
        }
        if (total > max) max = total
        return
    }
    for (i = 1; i <= n; i++) if (!used[i]) {
        used[i] = 1
        order[depth + 1] = list[i]
        dfs(depth + 1)
        used[i] = 0
    }
}
