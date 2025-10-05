#!/usr/bin/awk -f
END {
  line1 = ""; line2 = "";
  if ((getline line1 < "input.txt") <= 0) exit
  if ((getline line2 < "input.txt") <= 0) exit

  n1 = split(line1, a, ",")
  x = 0; y = 0
  for (i = 1; i <= n1; i++) {
    tok = a[i]
    dir = substr(tok, 1, 1)
    steps = substr(tok, 2) + 0
    for (s = 1; s <= steps; s++) {
      if (dir == "U") y++
      else if (dir == "D") y--
      else if (dir == "L") x--
      else if (dir == "R") x++
      key = x "," y
      vis[key] = 1
    }
  }

  n2 = split(line2, b, ",")
  x = 0; y = 0
  minDistance = 2147483647
  for (i = 1; i <= n2; i++) {
    tok = b[i]
    dir = substr(tok, 1, 1)
    steps = substr(tok, 2) + 0
    for (s = 1; s <= steps; s++) {
      if (dir == "U") y++
      else if (dir == "D") y--
      else if (dir == "L") x--
      else if (dir == "R") x++
      key = x "," y
      if ((key in vis) && vis[key] == 1) {
        dist = (x < 0 ? -x : x) + (y < 0 ? -y : y)
        if (dist < minDistance) minDistance = dist
      }
    }
  }
  print minDistance
}