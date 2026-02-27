#!/usr/bin/awk -f
BEGIN {
  fname = "input.txt"
  n = 0
  while ((getline line < fname) > 0) {
    m = split(line, a)
    if (m >= 5 && a[1] ~ /^\/dev\/grid\/node-x[0-9]+-y[0-9]+$/) {
      gsub(/T/, "", a[3])
      gsub(/T/, "", a[4])
      node_used[n] = a[3] + 0
      node_avail[n] = a[4] + 0
      n++
    }
  }
  close(fname)
}
END {
  count = 0
  for (i = 0; i < n; i++) {
    for (j = 0; j < n; j++) {
      if (i != j && node_used[i] > 0 && node_used[i] <= node_avail[j]) {
        count++
      }
    }
  }
  print count
}