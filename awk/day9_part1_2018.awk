BEGIN {
  filename = "input.txt"
  if ((getline line < filename) <= 0) { exit 1 }
  close(filename)

  split(line, P, /[[:space:]]+/)
  players = P[1] + 0
  lastMarble = P[7] + 0

  for (i = 1; i <= players; i++) S[i] = 0
  L[0] = 0
  R[0] = 0
  cur = 0

  for (m = 1; m <= lastMarble; m++) {
    if (m % 23 == 0) {
      for (s = 0; s < 7; s++) cur = L[cur]
      p = m % players
      if (p == 0) p = players
      S[p] += m + cur
      left = L[cur]
      right = R[cur]
      R[left] = right
      L[right] = left
      cur = right
    } else {
      cur = R[cur]
      L[m] = cur
      R[m] = R[cur]
      L[R[cur]] = m
      R[cur] = m
      cur = m
    }
  }

  max = 0
  for (i = 1; i <= players; i++) if (S[i] > max) max = S[i]
  print max
  exit
}