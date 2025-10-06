BEGIN {
  dx[1] = 1; dy[1] = 1
  dx[2] = 3; dy[2] = 1
  dx[3] = 5; dy[3] = 1
  dx[4] = 7; dy[4] = 1
  dx[5] = 1; dy[5] = 2

  lineCount = 0
  while ((getline s < "input.txt") > 0) {
    line[lineCount++] = s
  }
  close("input.txt")

  product = 1
  for (k = 1; k <= 5; k++) {
    trees = 0
    i = 0
    pos = 1
    dxk = dx[k]; dyk = dy[k]
    while (i < lineCount) {
      lineLen = length(line[i])
      if (substr(line[i], pos, 1) == "#") trees++
      pos = ((pos - 1 + dxk) % lineLen) + 1
      i += dyk
    }
    product *= trees
  }
  print product
  exit
}