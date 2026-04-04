
#!/usr/bin/env bash
awk '
BEGIN {
  while ("sort input.txt" | getline) {
    m = substr($2, 4, 2) + 0
    if ($3 == "Guard") {
      g = $4
      sub(/#/, "", g)
    } else if ($3 == "falls") {
      s = m
    } else if ($3 == "wakes") {
      for (i = s; i < m; i++) {
        cnt[g, i]++
        tot[g]++
      }
    }
  }
  for (g in tot) {
    if (tot[g] > max_tot) {
      max_tot = tot[g]
      best_g = g
    }
  }
  for (i = 0; i < 60; i++) {
    if (cnt[best_g, i] > max_f) {
      max_f = cnt[best_g, i]
      best_m = i
    }
  }
  print best_g * best_m
}
'
