
#!/bin/bash
awk -F'[^0-9-]+' '
/x=/ { n++; x[n]=$2; y[n]=$3; z[n]=$4 }
function gcd(a, b) {
  while (b) { t = b; b = a % b; a = t }
  return a
}
function lcm(a, b) {
  return (a / gcd(a, b)) * b
}
function solve(p, n,    v, i, j, c, k, q) {
  for (i = 1; i <= n; i++) { v[i] = 0; q[i] = p[i] }
  while (++c) {
    for (i = 1; i <= n; i++) {
      for (j = i + 1; j <= n; j++) {
        if (p[i] < p[j]) { v[i]++; v[j]-- }
        else if (p[i] > p[j]) { v[i]--; v[j]++ }
      }
    }
    k = 1
    for (i = 1; i <= n; i++) {
      p[i] += v[i]
      if (v[i] || p[i] != q[i]) k = 0
    }
    if (k) return c
  }
}
END {
  if (n) {
    rx = solve(x, n)
    ry = solve(y, n)
    rz = solve(z, n)
    printf "%.0f\n", lcm(rx, lcm(ry, rz))
  }
}' input.txt
