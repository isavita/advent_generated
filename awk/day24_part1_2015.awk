BEGIN {
  n = 0
  total = 0
  while ((getline line < "input.txt") > 0) {
    n++
    wts[n] = line + 0
    total += wts[n]
  }
  close("input.txt")
}
END {
  target = int(total / 3)
  for (k = 1; k <= n; k++) {
    bestQE = 0
    found = 0
    dfs(1, 0, 0, 1, k)
    if (found) {
      print bestQE
      exit
    }
  }
}
function dfs(start, depth, sum, prod, k,   i, maxIndex, w) {
  if (depth == k) {
    if (sum == target) {
      if (found == 0 || prod < bestQE) {
        bestQE = prod
        found = 1
      }
    }
    return
  }
  maxIndex = n - (k - depth) + 1
  for (i = start; i <= maxIndex; i++) {
    w = wts[i]
    if (sum + w <= target) dfs(i + 1, depth + 1, sum + w, prod * w, k)
  }
}