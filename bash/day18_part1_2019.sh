
awk '{
  for (x = 1; x <= length($0); x++) {
    v = substr($0, x, 1); g[NR, x] = v
    if (v == "@") { sy = NR; sx = x; g[NR, x] = "." }
    if (v ~ /[a-z]/ && !(v in k)) { k[v] = kc; b[kc] = 2^kc; kc++ }
  }
}
END {
  t = 2^kc - 1; q[h++] = sy FS sx FS 0 FS 0; vis[sy, sx, 0] = 1
  while (i < h) {
    split(q[i++], c, FS); y = c[1]; x = c[2]; m = c[3]; d = c[4]
    if (m == t) { print d; exit }
    for (j = 0; j < 4; j++) {
      ny = y + (j==0?-1:j==1?1:0); nx = x + (j==2?-1:j==3?1:0)
      at = g[ny, nx]; nm = m
      if (at == "" || at == "#") continue
      if (at ~ /[a-z]/) {
        ki = k[at]; if (int(m / b[ki]) % 2 == 0) nm += b[ki]
      } else if (at ~ /[A-Z]/) {
        ki = k[tolower(at)]; if (int(m / b[ki]) % 2 == 0) continue
      }
      if (!vis[ny, nx, nm]++) q[h++] = ny FS nx FS nm FS (d + 1)
    }
  }
  print -1
}' input.txt
