#!/usr/bin/awk -f
BEGIN {
  dR[1] = -1; dC[1] = -1
  dR[2] = 0;  dC[2] = -1
  dR[3] = 1;  dC[3] = -1
  dR[4] = -1; dC[4] = 0
  dR[5] = 1;  dC[5] = 0
  dR[6] = -1; dC[6] = 1
  dR[7] = 0;  dC[7] = 1
  dR[8] = 1;  dC[8] = 1
  rowCount = 0
  colCount = 0
  while ((getline line < "input.txt") > 0) {
    rowCount++
    if (length(line) > colCount) colCount = length(line)
    for (i = 1; i <= length(line); i++) seat[rowCount","i] = substr(line, i, 1)
  }
  close("input.txt")
  while (1) {
    changes = 0
    for (r = 1; r <= rowCount; r++) {
      for (c = 1; c <= colCount; c++) {
        ch = seat[r","c]
        if (ch == "L") {
          if (countVisible(r, c) == 0) {
            newSeat[r","c] = "#"; changes++
          } else {
            newSeat[r","c] = ch
          }
        } else if (ch == "#") {
          if (countVisible(r, c) >= 5) {
            newSeat[r","c] = "L"; changes++
          } else {
            newSeat[r","c] = ch
          }
        } else {
          newSeat[r","c] = ch
        }
      }
    }
    for (r = 1; r <= rowCount; r++) {
      for (c = 1; c <= colCount; c++) seat[r","c] = newSeat[r","c]
    }
    if (changes == 0) break
  }
  total = 0
  for (r = 1; r <= rowCount; r++) {
    for (c = 1; c <= colCount; c++) if (seat[r","c] == "#") total++
  }
  print total
  exit
}
function countVisible(r, c,   i, rr, cc, ch) {
  count = 0
  for (i = 1; i <= 8; i++) {
    rr = r + dR[i]
    cc = c + dC[i]
    while (rr >= 1 && rr <= rowCount && cc >= 1 && cc <= colCount) {
      ch = seat[rr","cc]
      if (ch == "L") break
      if (ch == "#") { count++; break }
      rr += dR[i]
      cc += dC[i]
    }
  }
  return count
}