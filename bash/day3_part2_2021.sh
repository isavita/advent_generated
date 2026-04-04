
#!/usr/bin/env bash
awk '
function d(s,    i, v) {
  v = 0
  for (i = 1; i <= length(s); i++) v = v * 2 + substr(s, i, 1)
  return v
}
function f(m,    i, j, c1, c0, t, rem, keep) {
  for (i = 1; i <= n; i++) keep[i] = a[i]
  rem = n
  for (j = 1; j <= L && rem > 1; j++) {
    c1 = 0
    for (i in keep) c1 += substr(keep[i], j, 1)
    c0 = rem - c1
    t = (m == "o" ? (c1 >= c0 ? "1" : "0") : (c0 <= c1 ? "0" : "1"))
    for (i in keep) {
      if (substr(keep[i], j, 1) != t) {
        delete keep[i]
        rem--
      }
    }
  }
  for (i in keep) return keep[i]
}
{
  a[++n] = $0
  L = length($0)
}
END {
  print d(f("o")) * d(f("c"))
}
' input.txt
