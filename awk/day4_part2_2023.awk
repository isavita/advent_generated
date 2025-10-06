#!/usr/bin/awk -f
function process_line(line) {
  pos = index(line, ":")
  if (pos <= 0) return
  cardDataStr = substr(line, pos + 2)
  sep = index(cardDataStr, " | ")
  if (sep > 0) {
    len_winnings = sep - 1
    pre = substr(cardDataStr, 1, len_winnings)
    post = substr(cardDataStr, sep + 3)
  } else {
    pre = cardDataStr
    post = ""
  }
  cardCount++
  cid = cardCount

  wstr = pre
  gsub(/[^0-9]+/, " ", wstr)
  nW = split(wstr, wnum, " ")
  wcnt = 0
  for (i = 1; i <= nW; i++) {
    if (wnum[i] != "") {
      wcnt++
      winnings[cid, wcnt] = wnum[i] + 0
    }
  }
  winnings_count[cid] = wcnt

  gstr = post
  gsub(/[^0-9]+/, " ", gstr)
  nG = split(gstr, gnum, " ")
  gcnt = 0
  for (i = 1; i <= nG; i++) {
    if (gnum[i] != "") {
      gcnt++
      givens[cid, gcnt] = gnum[i] + 0
    }
  }
  givens_count[cid] = gcnt

  totalCount[cid] = 1
}

BEGIN {
  fname = "input.txt"
  cardCount = 0
  while ((getline line < fname) > 0) {
    gsub(/\r$/, "", line)
    if (line ~ /^[[:space:]]*$/) continue
    process_line(line)
  }
  close(fname)

  for (i = 1; i <= cardCount; i++) {
    wcnt = winnings_count[i]
    gcnt = givens_count[i]
    p = 0
    if (gcnt > 0 && wcnt > 0) {
      for (g = 1; g <= gcnt; g++) {
        gv = givens[i, g]
        for (s = 1; s <= wcnt; s++) {
          if (gv == winnings[i, s]) { p++; break }
        }
      }
    }
    points[i] = p
  }

  for (i = 1; i <= cardCount; i++) {
    if (points[i] > 0) {
      for (j = 1; j <= points[i] && i + j <= cardCount; j++) {
        totalCount[i + j] += totalCount[i]
      }
    }
  }

  total = 0
  for (i = 1; i <= cardCount; i++) total += totalCount[i]
  printf("%d\n", total)
  exit
}