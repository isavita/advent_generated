
#!/usr/bin/env bash

awk '
BEGIN {
    dr[1]=0; dc[1]=1; dr[2]=0; dc[2]=-1; dr[3]=1; dc[3]=0; dr[4]=-1; dc[4]=0
}
{
    H++
    W = length($0)
    for (x = 1; x <= W; x++) g[H, x] = substr($0, x, 1)
}
END {
    for (x = 1; x <= W; x++) {
        if (g[1, x] == ".") { sy = 1; sx = x }
        if (g[H, x] == ".") { ey = H; ex = x }
    }

    for (r = 1; r <= H; r++) {
        for (c = 1; c <= W; c++) {
            if (g[r, c] == "#") continue
            deg = 0
            for (i = 1; i <= 4; i++) {
                nr = r + dr[i]; nc = c + dc[i]
                if (g[nr, nc] != "" && g[nr, nc] != "#") deg++
            }
            if (deg > 2 || r == 1 || r == H) {
                key = r "," c
                is_node[key] = 1
                node_to_id[key] = ++node_id
                id_r[node_id] = r
                id_c[node_id] = c
            }
        }
    }

    start_id = node_to_id[sy "," sx]
    target_id = node_to_id[ey "," ex]

    for (uid = 1; uid <= node_id; uid++) {
        r = id_r[uid]; c = id_c[uid]
        for (i = 1; i <= 4; i++) {
            nr = r + dr[i]; nc = c + dc[i]
            if (g[nr, nc] == "" || g[nr, nc] == "#") continue
            pr = r; pc = c; cr = nr; cc = nc; dist = 1
            while (!((cr "," cc) in is_node)) {
                for (j = 1; j <= 4; j++) {
                    tr = cr + dr[j]; tc = cc + dc[j]
                    if (g[tr, tc] != "" && g[tr, tc] != "#" && (tr != pr || tc != pc)) {
                        pr = cr; pc = cc; cr = tr; cc = tc
                        dist++
                        break
                    }
                }
            }
            vid = node_to_id[cr "," cc]
            adj[uid, ++adj_cnt[uid]] = vid
            weight[uid, vid] = dist
        }
    }

    print dfs(start_id, target_id, 0)
}
function dfs(u, target, d,    i, v, res, best) {
    if (u == target) return d
    vis[u] = 1
    best = -1
    for (i = 1; i <= adj_cnt[u]; i++) {
        v = adj[u, i]
        if (!vis[v]) {
            res = dfs(v, target, d + weight[u, v])
            if (res > best) best = res
        }
    }
    vis[u] = 0
    return best
}
' input.txt
