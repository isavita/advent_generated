
#!/usr/bin/env bash
awk '
function c2b(c) { return index("abcdefghijklmnopqrstuvwxyz", c) - 1 }
function tl(c) { return index("ABCDEFGHIJKLMNOPQRSTUVWXYZ", c) ? substr("abcdefghijklmnopqrstuvwxyz", index("ABCDEFGHIJKLMNOPQRSTUVWXYZ", c), 1) : c }
function bit(mask, b) { return int(mask / p2[b]) % 2 }
function setbit(m, b) { return bit(m, b) ? m : m + p2[b] }
function canpass(rq, gt, b) {
    if (rq == 0) return 1
    for (b = 0; b < 26; b++) {
        if (p2[b] > rq) break
        if (bit(rq, b) && !bit(gt, b)) return 0
    }
    return 1
}
BEGIN {
    FS = "\n"
    while ((getline < "input.txt") > 0) {
        line[++h] = $0
        w = length($0)
        for (x = 1; x <= w; x++) g[h, x] = substr($0, x, 1)
    }
    for (i = 0; i <= 30; i++) p2[i] = 2^i

    for (y = 2; y < h; y++) {
        for (x = 2; x < w; x++) {
            if (g[y, x] == "@" && g[y-1, x] == "." && g[y+1, x] == "." && g[y, x-1] == "." && g[y, x+1] == ".") {
                g[y-1, x-1] = "@"; g[y-1, x] = "#"; g[y-1, x+1] = "@"
                g[y, x-1] = "#";   g[y, x] = "#";   g[y, x+1] = "#"
                g[y+1, x-1] = "@"; g[y+1, x] = "#"; g[y+1, x+1] = "@"
                break
            }
        }
    }

    for (y = 1; y <= h; y++) {
        for (x = 1; x <= w; x++) {
            if (g[y, x] == "@") {
                ++cnt
                ix[cnt] = x
                iy[cnt] = y
            }
        }
    }
    robots = cnt

    for (y = 1; y <= h; y++) {
        for (x = 1; x <= w; x++) {
            if (g[y, x] ~ /[a-z]/) {
                ++cnt
                ix[cnt] = x
                iy[cnt] = y
                ch2id[g[y, x]] = cnt
                kb = c2b(g[y, x])
                kbit[cnt] = kb
                if (!bit(target, kb)) target += p2[kb]
            }
        }
    }

    dx[1] = 1; dy[1] = 0
    dx[2] = -1; dy[2] = 0
    dx[3] = 0; dy[3] = 1
    dx[4] = 0; dy[4] = -1

    for (i = 1; i <= cnt; i++) {
        delete vis
        for (k in qx) delete qx[k]
        head = tail = 0
        qx[++tail] = ix[i]
        qy[tail] = iy[i]
        qd[tail] = 0
        qm[tail] = 0
        vis[ix[i], iy[i]] = 1

        while (head < tail) {
            x = qx[++head]; y = qy[head]; d = qd[head]; m = qm[head]
            ch = g[y, x]
            base = m
            if (ch ~ /[a-z]/ && i != ch2id[ch]) {
                tid = ch2id[ch]
                adj[i, tid] = d
                req[i, tid] = m
                if (i <= robots) key_robot[tid] = i
                base = setbit(m, c2b(ch))
            }
            for (k = 1; k <= 4; k++) {
                nx = x + dx[k]; ny = y + dy[k]
                if (nx < 1 || nx > w || ny < 1 || ny > h) continue
                nc = g[ny, nx]
                if (nc != "#" && !((nx, ny) in vis)) {
                    vis[nx, ny] = 1
                    nm = base
                    if (nc ~ /[A-Z]/) nm = setbit(nm, c2b(tl(nc)))
                    qx[++tail] = nx
                    qy[tail] = ny
                    qd[tail] = d + 1
                    qm[tail] = nm
                }
            }
        }
    }

    st = ""
    for (i = 1; i <= 4; i++) st = st (i <= robots ? i : 0) ","
    st = st "0"
    dist[st] = 0
    q[0,1] = st
    qp[0] = 1
    maxc = 0

    for (c = 0; c <= maxc; c++) {
        if (!qp[c]) continue
        for (qi = 1; qi <= qp[c]; qi++) {
            s = q[c, qi]
            if (dist[s] < c) continue
            split(s, p, ",")
            mask = p[5] + 0
            if (mask == target) { print c; exit }
            for (r = 1; r <= 4; r++) {
                fid = p[r] + 0
                if (fid == 0) continue
                for (tid = robots + 1; tid <= cnt; tid++) {
                    if (key_robot[tid] != r || bit(mask, kbit[tid])) continue
                    if (adj[fid, tid] && canpass(req[fid, tid], mask)) {
                        nm = mask + p2[kbit[tid]]
                        ns = (r==1?tid:p[1]) "," (r==2?tid:p[2]) "," (r==3?tid:p[3]) "," (r==4?tid:p[4]) "," nm
                        nc = c + adj[fid, tid]
                        if (!(ns in dist) || nc < dist[ns]) {
                            dist[ns] = nc
                            q[nc, ++qp[nc]] = ns
                            if (nc > maxc) maxc = nc
                        }
                    }
                }
            }
        }
        delete qp[c]
    }
}
' </dev/null
