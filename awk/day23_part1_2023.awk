
BEGIN {
    ARGC = 2
    ARGV[1] = "input.txt"
    dr[0] = -1; dc[0] = 0
    dr[1] = 1; dc[1] = 0
    dr[2] = 0; dc[2] = -1
    dr[3] = 0; dc[3] = 1
}
{
    grid[NR - 1] = $0
    R = NR
}
function is_valid_basic(r, c) {
    return r >= 0 && r < R && c >= 0 && c < C && substr(grid[r], c + 1, 1) != "#"
}
function is_valid(r, c, d_r, d_c,    ch) {
    if (!is_valid_basic(r, c)) return 0
    ch = substr(grid[r], c + 1, 1)
    if (ch == ".") return 1
    if (ch == "^") return d_r == -1
    if (ch == "v") return d_r == 1
    if (ch == "<") return d_c == -1
    if (ch == ">") return d_c == 1
    return 0
}
function get_edges_bfs(u,    pos, head, tail, qr, qc, qd, is_v, r, c, d, i, nr, nc, cur, v) {
    split(nodes[u], pos, ",")
    head = 1; tail = 1
    qr[tail] = pos[1]; qc[tail] = pos[2]; qd[tail] = 0; tail++
    is_v[pos[1] "," pos[2]] = 1
    while (head < tail) {
        r = qr[head]; c = qc[head]; d = qd[head]; head++
        cur = r "," c
        if (cur in node_id && (r != pos[1] || c != pos[2])) {
            v = node_id[cur]
            adj_count[u]++
            adj_v[u, adj_count[u]] = v
            adj_w[u, adj_count[u]] = d
            continue
        }
        for (i = 0; i < 4; i++) {
            nr = r + dr[i]; nc = c + dc[i]
            if (is_valid(nr, nc, dr[i], dc[i]) && !( (nr "," nc) in is_v )) {
                is_v[nr "," nc] = 1
                qr[tail] = nr; qc[tail] = nc; qd[tail] = d + 1; tail++
            }
        }
    }
}
function dfs(u, cur_d,    v_idx, v) {
    if (u == 1) {
        if (cur_d > max_d) max_d = cur_d
        return
    }
    visited[u] = 1
    for (v_idx = 1; v_idx <= adj_count[u]; v_idx++) {
        v = adj_v[u, v_idx]
        if (!visited[v]) dfs(v, cur_d + adj_w[u, v_idx])
    }
    visited[u] = 0
}
END {
    C = length(grid[0])
    s_r = 0; s_c = 1; e_r = R - 1; e_c = C - 2
    node_id[s_r "," s_c] = 0; nodes[0] = s_r "," s_c
    node_id[e_r "," e_c] = 1; nodes[1] = e_r "," e_c
    node_count = 2
    for (r = 0; r < R; r++) {
        for (c = 0; c < C; c++) {
            if (substr(grid[r], c + 1, 1) == ".") {
                neighs = 0
                for (i = 0; i < 4; i++) if (is_valid_basic(r + dr[i], c + dc[i])) neighs++
                if (neighs > 2) {
                    p = r "," c
                    if (!(p in node_id)) { node_id[p] = node_count; nodes[node_count++] = p }
                }
            }
        }
    }
    for (u = 0; u < node_count; u++) get_edges_bfs(u)
    dfs(0, 0)
    print max_d
}
