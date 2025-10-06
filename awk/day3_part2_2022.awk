#!/usr/bin/awk -f
BEGIN {
  sum = 0
  gline = 0
  while ((getline line < "input.txt") > 0) {
    for (i = 1; i <= length(line); i++) {
      ch = substr(line, i, 1)
      if (ch ~ /[A-Za-z]/) {
        if (gline == 0) seen1[ch] = 1
        else if (gline == 1) seen2[ch] = 1
        else if (gline == 2) seen3[ch] = 1
      }
    }
    gline++
    if (gline == 3) {
      pr = 0
      for (c = 65; c <= 90; c++) {
        ch = sprintf("%c", c)
        if (seen1[ch] && seen2[ch] && seen3[ch]) { pr = 27 + (c - 65); break }
      }
      if (pr == 0) {
        for (c = 97; c <= 122; c++) {
          ch = sprintf("%c", c)
          if (seen1[ch] && seen2[ch] && seen3[ch]) { pr = (c - 97) + 1; break }
        }
      }
      sum += pr
      delete seen1
      delete seen2
      delete seen3
      gline = 0
    }
  }
  print sum
  exit
}