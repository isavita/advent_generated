
#!/usr/bin/env bash

awk '
function sortstr(w,    a,n,i,j,t,r) {
  n = split(w, a, "")
  for (i = 1; i < n; i++)
    for (j = i + 1; j <= n; j++)
      if (a[i] > a[j]) { t = a[i]; a[i] = a[j]; a[j] = t }
  r = ""
  for (i = 1; i <= n; i++) r = r a[i]
  return r
}

function hasall(a, b,    i,c) {
  if (!b) return 0
  for (i = 1; i <= length(b); i++) {
    c = substr(b, i, 1)
    if (index(a, c) == 0) return 0
  }
  return 1
}

BEGIN { ARGV[1] = "input.txt"; ARGC = 2 }

{
  delete d
  delete v

  for (i = 1; i <= 10; i++) {
    p[i] = sortstr($i)
    l = length(p[i])
    if (l == 2) d[1] = p[i]
    else if (l == 3) d[7] = p[i]
    else if (l == 4) d[4] = p[i]
    else if (l == 7) d[8] = p[i]
  }

  for (i = 1; i <= 10; i++) {
    l = length(p[i])
    if (l == 6) {
      if (hasall(p[i], d[4])) d[9] = p[i]
      else if (hasall(p[i], d[1])) d[0] = p[i]
      else d[6] = p[i]
    }
  }

  for (i = 1; i <= 10; i++) {
    l = length(p[i])
    if (l == 5) {
      if (hasall(p[i], d[1])) d[3] = p[i]
      else if (hasall(d[6], p[i])) d[5] = p[i]
      else d[2] = p[i]
    }
  }

  for (i = 0; i <= 9; i++) v[d[i]] = i

  ans += v[sortstr($12)] * 1000 + v[sortstr($13)] * 100 + v[sortstr($14)] * 10 + v[sortstr($15)]
}

END { print ans + 0 }
' 
