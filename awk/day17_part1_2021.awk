#!/usr/bin/awk -f
BEGIN {
  file = "input.txt"
  if ((getline line < file) <= 0) {
    print "Error opening file" > "/dev/stderr"
    exit 1
  }
  gsub(/[^0-9-]+/, " ", line)
  n = split(line, a, " ")
  if (n < 4) {
    print "Parse error" > "/dev/stderr"
    exit 1
  }
  xMin = a[1] + 0; xMax = a[2] + 0; yMin = a[3] + 0; yMax = a[4] + 0

  maxY = -1e9
  for (xVel = -1000; xVel <= 1000; xVel++) {
    for (yVel = -1000; yVel <= 1000; yVel++) {
      xPos = 0; yPos = 0
      curXVel = xVel; curYVel = yVel
      highestY = 0
      while (1) {
        xPos += curXVel
        yPos += curYVel

        if (xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax) {
          if (highestY > maxY) maxY = highestY
          break
        }

        if ((xPos < xMin && curXVel < 0) || (xPos > xMax && curXVel > 0) || (yPos < yMin && curYVel < 0)) {
          break
        }

        if (curXVel > 0) curXVel--
        else if (curXVel < 0) curXVel++
        curYVel--

        if (yPos > highestY) highestY = yPos
      }
    }
  }

  print maxY
}