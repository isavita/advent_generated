BEGIN {
  if ((getline line1 < "input.txt") <= 0) exit
  if ((getline line2 < "input.txt") <= 0) exit

  n1 = split(line1, a1, /[ \t]+/)
  timeCount = 0
  for (i = 1; i <= n1; i++) {
    if (a1[i] != "") times[++timeCount] = a1[i] + 0
  }

  n2 = split(line2, a2, /[ \t]+/)
  distCount = 0
  for (i = 1; i <= n2; i++) {
    if (a2[i] != "") distances[++distCount] = a2[i] + 0
  }

  totalWays = 1
  for (i = 1; i <= timeCount; i++) {
    totalWays *= calculateWaysToWin(times[i], distances[i])
  }
  print totalWays
  exit
}
function calculateWaysToWin(time, record,   low, high, mid, dist) {
  low = 1
  high = time - 1
  while (low <= high) {
    mid = int(low + (high - low) / 2)
    dist = mid * (time - mid)
    if (dist > record) {
      high = mid - 1
    } else {
      low = mid + 1
    }
  }
  return time - 2 * low + 1
}