
#!/usr/bin/env bash
awk '
BEGIN {
  dx["R"]=1; dx["L"]=-1; dy["U"]=-1; dy["D"]=1
  while ((getline line < "input.txt") > 0) {
    W = length(line)
    H++
    for (i = 1; i <= W; i++) g[H-1, i-1] = substr(line, i, 1)
  }
  close("input.txt")
}

function push(nx, ny, nd) {
  if (nx >= 0 && nx < W && ny >= 0 && ny < H) q[++t] = nx "," ny "," nd
}

END {
  push(0, 0, "R")
  while (t > 0) {
    c = q[t--]
    if (v[c]++) continue
    split(c, a, ",")
    x = a[1]; y = a[2]; d = a[3]
    e[x, y] = 1
    c = g[y, x]
    if (c == ".") {
      push(x + dx[d], y + dy[d], d)
    } else if (c == "/") {
      n = (d == "R" ? "U" : d == "L" ? "D" : d == "U" ? "R" : "L")
      push(x + dx[n], y + dy[n], n)
    } else if (c == "\\") {
      n = (d == "R" ? "D" : d == "L" ? "U" : d == "U" ? "L" : "R")
      push(x + dx[n], y + dy[n], n)
    } else if (c == "|") {
      if (d == "R" || d == "L") {
        push(x, y - 1, "U")
        push(x, y + 1, "D")
      } else {
        push(x + dx[d], y + dy[d], d)
      }
    } else if (c == "-") {
      if (d == "U" || d == "D") {
        push(x - 1, y, "L")
        push(x + 1, y, "R")
      } else {
        push(x + dx[d], y + dy[d], d)
      }
    }
  }
  for (i in e) r++
  print r + 0
}
' </dev/null
