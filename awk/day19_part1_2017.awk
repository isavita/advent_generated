#!/usr/bin/awk -f
BEGIN {
  rows = 0; cols = 0;
  while ((getline line < "input.txt") > 0) {
    rows++;
    grid[rows] = line;
    if (length(line) > cols) cols = length(line);
  }
  close("input.txt");

  line0 = grid[1];
  start_x = 0;
  for (i = 1; i <= length(line0); i++) {
    if (substr(line0, i, 1) == "|") { start_x = i; break; }
  }

  x = start_x; y = 1; dx = 0; dy = 1;
  letters = "";

  while (1) {
    if (y < 1 || y > rows) break;
    line = grid[y];
    if (x < 1 || x > length(line)) break;
    cell = substr(line, x, 1);
    if (cell == " ") break;
    if (cell >= "A" && cell <= "Z") letters = letters cell;

    if (cell == "+") {
      if (dx == 0) {
        leftChar = (x > 1) ? substr(line, x-1, 1) : "";
        if (x > 1 && (leftChar == "-" || (leftChar >= "A" && leftChar <= "Z"))) {
          dx = -1;
        } else {
          dx = 1;
        }
        dy = 0;
      } else {
        upChar = (y > 1) ? substr(grid[y-1], x, 1) : "";
        if (y > 1 && (upChar == "|" || (upChar >= "A" && upChar <= "Z"))) {
          dy = -1;
        } else {
          dy = 1;
        }
        dx = 0;
      }
    }

    x += dx;
    y += dy;
  }

  print letters;
  exit;
}