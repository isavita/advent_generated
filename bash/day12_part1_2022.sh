
#!/bin/bash
awk '
BEGIN {
  for (i = 97; i <= 122; i++) h[sprintf("%c", i)] = i - 97
}
{
  for (i = 1; i <= length($0); i++) {
    c = substr($0, i, 1)
    if (c == "S") { sx = i - 1; sy = NR - 1; c = "a" }
    if (c == "E") { ex = i - 1; ey = NR - 1; c = "z" }
    g[NR - 1, i - 1] = h[c]
  }
  w = length($0); r = NR
}
END {
  split("0 0 1 -1", dx); split("1 -1 0 0", dy)
  q[0] = ey SUBSEP ex; d[ey, ex] = 0; head = 0; tail = 0
  while (head <= tail) {
    split(q[head++], p, SUBSEP); y = p[1]; x = p[2]
    if (x == sx && y == sy) { print d[y, x]; exit }
    for (i = 1; i <= 4; i++) {
      ny = y + dy[i]; nx = x + dx[i]
      if (nx >= 0 && nx < w && ny >= 0 && ny < r && !((ny, nx) in d)) {
        if (g[y, x] - g[ny, nx] <= 1) {
          d[ny, nx] = d[y, x] + 1
          q[++tail] = ny SUBSEP nx
        }
      }
    }
  }
}' input.txt
