#!/usr/bin/awk -f
BEGIN {
  line1 = ""; line2 = ""
  if ((getline line1 < "input.txt") <= 0) exit
  if ((getline line2 < "input.txt") <= 0) exit
  close("input.txt")

  # Process first line and store minimal steps to reach each coordinate
  x = 0; y = 0; steps = 0
  n1 = split(line1, seg1, ",")
  delete first
  for (s = 1; s <= n1; s++) {
    token = seg1[s]
    dir = substr(token, 1, 1)
    dist = int(substr(token, 2))
    for (i = 1; i <= dist; i++) {
      steps++
      if (dir == "U") y++
      else if (dir == "D") y--
      else if (dir == "L") x--
      else if (dir == "R") x++
      coord = x "," y
      if (!(coord in first)) first[coord] = steps
    }
  }

  # Process second line and compute minimal combined steps at intersections
  x = 0; y = 0; steps = 0
  minSteps = 1e9
  n2 = split(line2, seg2, ",")
  for (s = 1; s <= n2; s++) {
    token = seg2[s]
    dir = substr(token, 1, 1)
    dist = int(substr(token, 2))
    for (i = 1; i <= dist; i++) {
      steps++
      if (dir == "U") y++
      else if (dir == "D") y--
      else if (dir == "L") x--
      else if (dir == "R") x++
      coord = x "," y
      if (coord in first) {
        total = steps + first[coord]
        if (total < minSteps) minSteps = total
      }
    }
  }

  if (minSteps == 1e9) minSteps = 0
  print minSteps
  exit
}