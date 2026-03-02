
function v(p) { return prog[p]+0 }
function step(input,   ins, op, m1, m2, m3, p1, p2, p3, out) {
    while (1) {
        ins = prog[pc]; op = ins % 100
        m1 = int(ins / 100) % 10; m2 = int(ins / 1000) % 10; m3 = int(ins / 10000) % 10
        p1 = (m1 == 1 ? pc + 1 : (m1 == 2 ? rel + prog[pc + 1] : prog[pc + 1]))
        p2 = (m2 == 1 ? pc + 2 : (m2 == 2 ? rel + prog[pc + 2] : prog[pc + 2]))
        p3 = (m3 == 1 ? pc + 3 : (m3 == 2 ? rel + prog[pc + 3] : prog[pc + 3]))
        if (op == 1) { prog[p3] = v(p1) + v(p2); pc += 4 }
        else if (op == 2) { prog[p3] = v(p1) * v(p2); pc += 4 }
        else if (op == 3) { prog[p1] = input; pc += 2 }
        else if (op == 4) { out = v(p1); pc += 2; return out }
        else if (op == 5) { pc = (v(p1) != 0 ? v(p2) : pc + 3) }
        else if (op == 6) { pc = (v(p1) == 0 ? v(p2) : pc + 3) }
        else if (op == 7) { prog[p3] = (v(p1) < v(p2) ? 1 : 0); pc += 4 }
        else if (op == 8) { prog[p3] = (v(p1) == v(p2) ? 1 : 0); pc += 4 }
        else if (op == 9) { rel += v(p1); pc += 2 }
        else if (op == 99) return -1
    }
}
function dfs(x, y,   d, nx, ny, res, key) {
    visited[x "," y] = 1
    for (d = 1; d <= 4; d++) {
        nx = x + dx[d]; ny = y + dy[d]; key = nx "," ny
        if (!(key in visited)) {
            res = step(d)
            visited[key] = 1; grid[key] = res
            if (res > 0) {
                if (res == 2) { tx = nx; ty = ny }
                dfs(nx, ny)
                step(opp[d])
            }
        }
    }
}
function bfs(sx, sy, tx, ty,   head, tail, qx, qy, qd, cx, cy, cd, i, nx, ny, key, v_bfs) {
    head = tail = 1; qx[1] = sx; qy[1] = sy; qd[1] = 0; v_bfs[sx "," sy] = 1
    while (head <= tail) {
        cx = qx[head]; cy = qy[head]; cd = qd[head++]; if (cx == tx && cy == ty) return cd
        for (i = 1; i <= 4; i++) {
            nx = cx + dx[i]; ny = cy + dy[i]; key = nx "," ny
            if (grid[key] > 0 && !(key in v_bfs)) {
                v_bfs[key] = 1; tail++; qx[tail] = nx; qy[tail] = ny; qd[tail] = cd + 1
            }
        }
    }
}
BEGIN {
    while ((getline < "input.txt") > 0) {
        n = split($0, a, ",")
        for (i = 1; i <= n; i++) prog[i - 1] = a[i]
    }
    pc = rel = 0
    dx[1] = 0; dy[1] = -1; dx[2] = 0; dy[2] = 1; dx[3] = -1; dy[3] = 0; dx[4] = 1; dy[4] = 0
    opp[1] = 2; opp[2] = 1; opp[3] = 4; opp[4] = 3
    grid["0,0"] = 1; dfs(0, 0)
    print bfs(0, 0, tx, ty)
}
