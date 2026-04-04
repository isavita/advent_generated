
#!/usr/bin/env bash
set -euo pipefail

awk '
{
  n = split($0, a, "")
  for (c = 1; c <= n; c++) g[NR, c] = a[c] + 0
  nc = n
}
END {
  sum = 0
  for (r = 1; r <= NR; r++) for (c = 1; c <= nc; c++) if (g[r, c] == 0) {
    delete vis
    delete reach
    h = t = 1
    qr[h] = r
    qc[h] = c
    qv[h] = 0
    vis[r SUBSEP c SUBSEP 0] = 1
    while (h <= t) {
      cr = qr[h]
      cc = qc[h]
      ch = qv[h]
      h++
      if (ch == 9) {
        reach[cr SUBSEP cc] = 1
      } else {
        nh = ch + 1
        if (cr > 1 && g[cr - 1, cc] == nh && !((cr - 1 SUBSEP cc SUBSEP nh) in vis)) {
          qr[++t] = cr - 1; qc[t] = cc; qv[t] = nh
          vis[cr - 1 SUBSEP cc SUBSEP nh] = 1
        }
        if (cr < NR && g[cr + 1, cc] == nh && !((cr + 1 SUBSEP cc SUBSEP nh) in vis)) {
          qr[++t] = cr + 1; qc[t] = cc; qv[t] = nh
          vis[cr + 1 SUBSEP cc SUBSEP nh] = 1
        }
        if (cc > 1 && g[cr, cc - 1] == nh && !((cr SUBSEP cc - 1 SUBSEP nh) in vis)) {
          qr[++t] = cr; qc[t] = cc - 1; qv[t] = nh
          vis[cr SUBSEP cc - 1 SUBSEP nh] = 1
        }
        if (cc < nc && g[cr, cc + 1] == nh && !((cr SUBSEP cc + 1 SUBSEP nh) in vis)) {
          qr[++t] = cr; qc[t] = cc + 1; qv[t] = nh
          vis[cr SUBSEP cc + 1 SUBSEP nh] = 1
        }
      }
    }
    for (k in reach) sum++
  }
  print sum
}
' input.txt
