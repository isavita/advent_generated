#!/usr/bin/awk -f
function abs(n) { return (n < 0 ? -n : n) }
function rotate(deg,   tmp) {
  deg = (deg % 360 + 360) % 360
  if (deg == 90) { tmp = wx; wx = wy; wy = -tmp }
  else if (deg == 180) { wx = -wx; wy = -wy }
  else if (deg == 270) { tmp = wx; wx = -wy; wy = tmp }
}
function process_action(action, n) {
  if (action == "N") wy += n
  else if (action == "S") wy -= n
  else if (action == "E") wx += n
  else if (action == "W") wx -= n
  else if (action == "L") rotate(-n)
  else if (action == "R") rotate(n)
  else if (action == "F") { x += wx * n; y += wy * n }
}
BEGIN {
  x = 0; y = 0; wx = 10; wy = 1
  line = ""
  while ((getline line < "input.txt") > 0) {
    action = substr(line, 1, 1)
    n = substr(line, 2) + 0
    process_action(action, n)
  }
  print abs(x) + abs(y)
  exit
}