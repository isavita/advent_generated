
#!/usr/bin/env bash
awk '
function intcode(inp,   instr, op, m1, m2, m3, a1, a2, a3, v1, v2) {
    while (1) {
        instr = m[p]; op = instr % 100
        m1 = int(instr / 100) % 10
        m2 = int(instr / 1000) % 10
        m3 = int(instr / 10000) % 10
        a1 = (m1 == 2 ? rb + m[p+1] : (m1 == 0 ? m[p+1] : p+1))
        v1 = m[a1]
        a2 = (m2 == 2 ? rb + m[p+2] : (m2 == 0 ? m[p+2] : p+2))
        v2 = m[a2]
        a3 = (m3 == 2 ? rb + m[p+3] : (m3 == 0 ? m[p+3] : p+3))
        if (op == 1) { m[a3] = v1 + v2; p += 4 }
        else if (op == 2) { m[a3] = v1 * v2; p += 4 }
        else if (op == 3) { m[a1] = inp; p += 2 }
        else if (op == 4) { p += 2; return v1 }
        else if (op == 5) p = (v1 != 0 ? v2 : p + 3)
        else if (op == 6) p = (v1 == 0 ? v2 : p + 3)
        else if (op == 7) { m[a3] = (v1 < v2 ? 1 : 0); p += 4 }
        else if (op == 8) { m[a3] = (v1 == v2 ? 1 : 0); p += 4 }
        else if (op == 9) { rb += v1; p += 2 }
        else if (op == 99) return -1
    }
}
function explore(x, y,   d, nx, ny, stat) {
    for (d = 1; d <= 4; d++) {
        nx = x + dx[d]; ny = y + dy[d]
        if (!((nx "," ny) in maze)) {
            stat = intcode(d)
            maze[nx "," ny] = stat
            if (stat != 0) {
                if (stat == 2) oxy_pos = nx "," ny
                explore(nx, ny)
                intcode(opp[d])
            }
        }
    }
}
function bfs(start, target,   q, head, tail, cur, coord, x, y, i, nx, ny, cp) {
    delete dist
    head = tail = 1; q[tail++] = start; dist[start] = 0; max_d = 0
    while (head < tail) {
        cur = q[head++]; split(cur, coord, ",")
        x = coord[1]; y = coord[2]
        for (i = 1; i <= 4; i++) {
            nx = x + dx[i]; ny = y + dy[i]; cp = nx "," ny
            if (maze[cp] != 0 && !(cp in dist)) {
                dist[cp] = dist[cur] + 1
                if (dist[cp] > max_d) max_d = dist[cp]
                q[tail++] = cp
            }
        }
    }
    return (target == "" ? max_d : dist[target])
}
BEGIN {
    RS = ","; while ((getline v < "input.txt") > 0) m[c++] = v + 0
    dx[1]=0; dy[1]=-1; dx[2]=0; dy[2]=1; dx[3]=-1; dy[3]=0; dx[4]=1; dy[4]=0
    opp[1]=2; opp[2]=1; opp[3]=4; opp[4]=3
    p = rb = 0; maze["0,0"] = 1
    explore(0,0)
    print bfs("0,0",oxy_pos)
    print bfs(oxy_pos,"")
}
' </dev/null
