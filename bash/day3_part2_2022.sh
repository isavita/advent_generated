
#!/usr/bin/env bash

awk '
BEGIN { sum = 0; g = 0 }
{
  split("", seen)
  for (i = 1; i <= length($0); i++) {
    ch = substr($0, i, 1)
    if (ch ~ /[A-Za-z]/) seen[ch] = 1
  }
  g++
  if (g == 1) for (c in seen) s1[c] = 1
  else if (g == 2) for (c in seen) s2[c] = 1
  else {
    pr = 0
    for (c = 65; c <= 90; c++) {
      ch = sprintf("%c", c)
      if (s1[ch] && s2[ch] && seen[ch]) { pr = 27 + c - 65; break }
    }
    if (pr == 0) {
      for (c = 97; c <= 122; c++) {
        ch = sprintf("%c", c)
        if (s1[ch] && s2[ch] && seen[ch]) { pr = c - 96; break }
      }
    }
    sum += pr
    delete s1
    delete s2
    g = 0
  }
}
END { print sum }
' input.txt
