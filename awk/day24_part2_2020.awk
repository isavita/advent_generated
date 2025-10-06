#!/usr/bin/awk -f
BEGIN {
  dx[0] = 1;  drs[0] = 0
  dx[1] = 0;  drs[1] = 1
  dx[2] = -1; drs[2] = 1
  dx[3] = -1; drs[3] = 0
  dx[4] = 0;  drs[4] = -1
  dx[5] = 1;  drs[5] = -1
  fid = "input.txt"
  FS = ","
  while ((getline line < fid) > 0) {
    q = 0; r = 0; i = 1; n = length(line)
    while (i <= n) {
      ch = substr(line, i, 1)
      if (ch == "e" || ch == "w") {
        if (ch == "e") { dq = 1; dr = 0 } else { dq = -1; dr = 0 }
        i++
      } else {
        ch2 = substr(line, i+1, 1)
        if (ch == "n") {
          if (ch2 == "w") { dq = 0; dr = -1 } else if (ch2 == "e") { dq = 1; dr = -1 }
        } else if (ch == "s") {
          if (ch2 == "e") { dq = 0; dr = 1 } else if (ch2 == "w") { dq = -1; dr = 1 }
        }
        i += 2
      }
      q += dq; r += dr
    }
    key = q "," r
    if (black[key] == 1) { delete black[key] } else { black[key] = 1 }
  }
  close(fid)

  for (day = 1; day <= 100; day++) {
    delete tilesToCheck
    for (k in black) {
      if (black[k] == 1) {
        tilesToCheck[k] = 1
        split(k, arr, ",")
        q0 = arr[1] + 0; r0 = arr[2] + 0
        for (t = 0; t < 6; t++) {
          nk = (q0 + dx[t]) "," (r0 + drs[t])
          tilesToCheck[nk] = 1
        }
      }
    }
    delete newBlack
    for (tk in tilesToCheck) {
      split(tk, arr, ",")
      tq = arr[1] + 0; tr = arr[2] + 0
      bc = 0
      for (t = 0; t < 6; t++) {
        nkey = (tq + dx[t]) "," (tr + drs[t])
        if (black[nkey] == 1) bc++
      }
      if (black[tk] == 1) {
        if (bc == 1 || bc == 2) newBlack[tk] = 1
      } else {
        if (bc == 2) newBlack[tk] = 1
      }
    }
    delete black
    for (nk in newBlack) black[nk] = newBlack[nk]
  }

  count = 0
  for (k in black) if (black[k] == 1) count++
  print count
  exit
}