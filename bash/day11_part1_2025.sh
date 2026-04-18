
#!/usr/bin/env bash
awk '
function dfs(node,   i, sum) {
  if (node == "out") return 1
  if (seen[node]) return memo[node]
  seen[node] = 1
  sum = 0
  for (i = 1; i <= deg[node]; i++) sum += dfs(adj[node, i])
  memo[node] = sum
  return sum
}
{
  gsub(/^[[:space:]]+|[[:space:]]+$/, "", $0)
  if ($0 == "") next
  split($0, p, ":")
  if (length(p) == 2) {
    src = p[1]
    gsub(/^[[:space:]]+|[[:space:]]+$/, "", src)
    n = split(p[2], a, /[[:space:]]+/)
    deg[src] = 0
    for (i = 1; i <= n; i++) if (a[i] != "") adj[src, ++deg[src]] = a[i]
  }
}
END {
  print dfs("you")
}
' input.txt
