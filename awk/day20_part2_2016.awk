#!/usr/bin/awk -f
BEGIN {
  MAX = 4294967295
  cnt = 0

  while ((getline line < "input.txt") > 0) {
    gsub(/^[ \t]+|[ \t]+$/, "", line)
    if (line == "") continue

    n = split(line, parts, "-")
    if (n == 2) {
      s = parts[1]
      t = parts[2]
      gsub(/^[ \t]+|[ \t]+$/, "", s)
      gsub(/^[ \t]+|[ \t]+$/, "", t)
      if (s ~ /^[0-9]+$/ && t ~ /^[0-9]+$/) {
        cnt++
        S[cnt] = s + 0
        E[cnt] = t + 0
      }
    }
  }
  close("input.txt")

  if (cnt == 0) {
    printf("4294967296\n")
    exit
  }

  # Insertion sort by start then end
  for (i = 1; i <= cnt; i++) {
    for (j = i + 1; j <= cnt; j++) {
      if (S[j] < S[i] || (S[j] == S[i] && E[j] < E[i])) {
        tmp = S[i]; S[i] = S[j]; S[j] = tmp
        tmp2 = E[i]; E[i] = E[j]; E[j] = tmp2
      }
    }
  }

  mergedN = 0
  for (i = 1; i <= cnt; i++) {
    if (mergedN == 0) {
      mergedN = 1
      MS[1] = S[i]
      ME[1] = E[i]
    } else {
      if (ME[mergedN] >= S[i] - 1) {
        if (E[i] > ME[mergedN]) ME[mergedN] = E[i]
      } else {
        mergedN++
        MS[mergedN] = S[i]
        ME[mergedN] = E[i]
      }
    }
  }

  if (ME[mergedN] != MAX) {
    mergedN++
    MS[mergedN] = MAX
    ME[mergedN] = 0
  }

  total = 0
  for (i = 2; i <= mergedN; i++) {
    total += MS[i] - ME[i-1] - 1
  }

  printf("%u\n", total)
  exit
}