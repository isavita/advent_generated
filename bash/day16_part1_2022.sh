
#!/usr/bin/env bash
awk '
BEGIN {
  while ((getline < "input.txt") > 0) {
    v = $2
    all[v] = 1
    split($5, a, /[=;]/)
    flow[v] = a[2]
    for (i = 10; i <= NF; i++) {
      t = $i
      gsub(/,/, "", t)
      dist[v, t] = 1
    }
    dist[v, v] = 0
  }

  for (k in all)
    for (i in all)
      if ((i, k) in dist)
        for (j in all)
          if ((k, j) in dist) {
            d = dist[i, k] + dist[k, j]
            if (!((i, j) in dist) || d < dist[i, j]) dist[i, j] = d
          }

  for (v in all)
    if (flow[v] > 0) useful[++n] = v

  solve("AA", 30, 0)
  print max_p
}

function solve(curr, time, score, i, target, cost) {
  if (score > max_p) max_p = score
  for (i = 1; i <= n; i++) {
    target = useful[i]
    if (!visited[target]) {
      cost = dist[curr, target] + 1
      if (time > cost) {
        visited[target] = 1
        solve(target, time - cost, score + (time - cost) * flow[target])
        visited[target] = 0
      }
    }
  }
}
' input.txt
