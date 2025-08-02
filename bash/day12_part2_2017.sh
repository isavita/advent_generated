#!/usr/bin/env bash
set -euo pipefail

awk -F' <-> ' '
{
  split($2, arr, ", ")
  for (i in arr) {
    a[$1, arr[i]] = 1
    a[arr[i], $1] = 1
    nodes[$1] = 1
    nodes[arr[i]] = 1
  }
}
END {
  for (n in nodes) visited[n] = 0
  groups = 0
  for (start in nodes) {
    if (visited[start]) continue
    groups++
    q[1] = start; head = 1; tail = 1
    visited[start] = 1
    while (head <= tail) {
      u = q[head++]
      for (v in nodes) {
        if (a[u, v] && !visited[v]) {
          visited[v] = 1
          q[++tail] = v
        }
      }
    }
    delete q
  }
  print groups
}
' input.txt