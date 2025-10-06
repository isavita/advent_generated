BEGIN {
  H = 0
  while ((getline line < "input.txt") > 0) {
    lines[H] = line
    H++
  }
  if (H == 0) { print 0; exit }
  W = length(lines[0])
  for (y = 0; y < H; y++) {
    line = lines[y]
    w = length(line)
    if (w > W) W = w
    for (x = 0; x < w; x++) {
      c = substr(line, x+1, 1)
      if (c != ".") {
        cnt[c]++
        pos_y[c, cnt[c]] = y
        pos_x[c, cnt[c]] = x
      }
    }
  }

  antinodesCount = 0
  for (sym in cnt) {
    n = cnt[sym]
    for (i = 1; i <= n - 1; i++) {
      Ay = pos_y[sym, i]; Ax = pos_x[sym, i]
      for (j = i + 1; j <= n; j++) {
        By = pos_y[sym, j]; Bx = pos_x[sym, j]
        p1y = 2*Ay - By
        p1x = 2*Ax - Bx
        if (p1y >= 0 && p1y < H && p1x >= 0 && p1x < W) {
          k = p1y "," p1x
          if (!(k in antinodes)) {
            antinodes[k] = 1
            antinodesCount++
          }
        }
        p2y = 2*By - Ay
        p2x = 2*Bx - Ax
        if (p2y >= 0 && p2y < H && p2x >= 0 && p2x < W) {
          k = p2y "," p2x
          if (!(k in antinodes)) {
            antinodes[k] = 1
            antinodesCount++
          }
        }
      }
    }
  }
  print antinodesCount
}