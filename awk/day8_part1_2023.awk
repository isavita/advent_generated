#!/usr/bin/awk -f
BEGIN {
  fname = "input.txt"
  if ((getline line < fname) <= 0) exit
  gsub(/\r$/, "", line)
  instructions = line

  if ((getline line < fname) <= 0) exit
  gsub(/\r$/, "", line)

  while ((getline line < fname) > 0) {
    gsub(/\r$/, "", line)
    n = split(line, f, /[^A-Z]+/)
    if (n >= 3) {
      name = f[1]
      l = f[2]
      r = f[3]
      if (name != "" && l != "" && r != "")
        left[name] = l
        right[name] = r
    }
  }

  current = "AAA"
  target = "ZZZ"
  steps = 0
  len = length(instructions)
  idx = 1
  while (current != target) {
    dir = substr(instructions, idx, 1)
    if (dir == "R") current = right[current]
    else current = left[current]
    steps++
    idx++
    if (idx > len) idx = 1
  }
  print steps
  exit
}