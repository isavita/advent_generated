
#!/usr/bin/env bash
set -euo pipefail

awk '
BEGIN {
  while ((getline line < "input.txt") > 0) {
    h++
    w = length(line)
    for (i = 1; i <= w; i++) g[h, i] = substr(line, i, 1)
    if (w > maxw) maxw = w
  }

  for (y = 1; y <= h; y++) {
    for (x = 1; x <= maxw; x++) {
      if (g[y, x] == ".") {
        if (minx == "") { minx = maxx = x; miny = maxy = y }
        if (x < minx) minx = x
        if (x > maxx) maxx = x
        if (y < miny) miny = y
        if (y > maxy) maxy = y
      }
    }
  }

  for (y = 1; y <= h; y++) {
    for (x = 1; x <= maxw; x++) {
      if (g[y, x] ~ /[A-Z]/) {
        if (g[y, x + 1] ~ /[A-Z]/) {
          l = g[y, x] g[y, x + 1]
          px = -1
          if (g[y, x - 1] == ".") { px = x - 1; py = y }
          else if (g[y, x + 2] == ".") { px = x + 2; py = y }
          if (px != -1) hp(l, px, py)
        }
        if (g[y + 1, x] ~ /[A-Z]/) {
          l = g[y, x] g[y + 1, x]
          px = -1
          if (g[y - 1, x] == ".") { px = x; py = y - 1 }
          else if (g[y + 2, x] == ".") { px = x; py = y + 2 }
          if (px != -1) hp(l, px, py)
        }
      }
    }
  }

  q[t++] = sx SUBSEP sy SUBSEP 0 SUBSEP 0
  v[sx, sy, 0] = 1

  d[0] = 1; d[1] = -1; d[2] = 0; d[3] = 0
  e[0] = 0; e[1] = 0; e[2] = 1; e[3] = -1

  while (s < t) {
    split(q[s++], c, SUBSEP)
    delete q[s - 1]
    cx = c[1]; cy = c[2]; cl = c[3]; cs = c[4]

    if (cx == ex && cy == ey && cl == 0) { print cs; exit }

    for (i = 0; i < 4; i++) {
      nx = cx + d[i]
      ny = cy + e[i]
      if (g[ny, nx] == "." && !v[nx, ny, cl]) {
        v[nx, ny, cl] = 1
        q[t++] = nx SUBSEP ny SUBSEP cl SUBSEP (cs + 1)
      }
    }

    if (j[cx, cy]) {
      split(j[cx, cy], tt, ",")
      nl = cl + jd[cx, cy]
      if (nl >= 0 && nl < 100 && !v[tt[1], tt[2], nl]) {
        v[tt[1], tt[2], nl] = 1
        q[t++] = tt[1] SUBSEP tt[2] SUBSEP nl SUBSEP (cs + 1)
      }
    }
  }
}

function hp(l, x, y) {
  if (l == "AA") {
    sx = x; sy = y
  } else if (l == "ZZ") {
    ex = x; ey = y
  } else if (l in tmp) {
    split(tmp[l], p, ",")
    x1 = p[1]; y1 = p[2]
    j[x, y] = x1 "," y1
    j[x1, y1] = x "," y
    jd[x, y] = (x == minx || x == maxx || y == miny || y == maxy ? -1 : 1)
    jd[x1, y1] = (x1 == minx || x1 == maxx || y1 == miny || y1 == maxy ? -1 : 1)
  } else {
    tmp[l] = x "," y
  }
}
' </dev/null
