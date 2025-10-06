#!/usr/bin/awk -f
function allZeroHist(len) {
  for (i = 1; i <= len; i++) if (hist[i] != 0) return 0
  return 1
}
BEGIN {
  fname = "input.txt"
  total = 0
  while ((getline line < fname) > 0) {
    if (length(line) == 0) continue
    n = split(line, tmp, /[ \t]+/)
    if (n == 0) continue
    for (i = 1; i <= n; i++) hist[i] = tmp[i] + 0
    flen = 1
    firsts[1] = hist[1]
    while (n > 1) {
      for (i = 1; i <= n - 1; i++) diff[i] = hist[i+1] - hist[i]
      firsts[++flen] = diff[1]
      for (i = 1; i <= n - 1; i++) hist[i] = diff[i]
      n = n - 1
      if (allZeroHist(n)) break
    }
    p = 0
    for (i = flen; i >= 1; i--) p = firsts[i] - p
    total += p
  }
  print total
  exit
}