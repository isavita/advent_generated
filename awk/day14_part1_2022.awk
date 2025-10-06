#!/usr/bin/awk -f
BEGIN {
  floor_level = 0;
  while ((getline line < "input.txt") > 0) {
    n = split(line, seg, / -> /);
    for (i = 1; i <= n; i++) {
      split(seg[i], c, ",");
      if (i == 1) {
        cx = c[1] + 0; cy = c[2] + 0;
      } else {
        nx = c[1] + 0; ny = c[2] + 0;
        mark_line(cx, cy, nx, ny);
        cx = nx; cy = ny;
      }
    }
  }
  close("input.txt");
  ans = do_fill();
  print ans;
  exit;
}
function mark_line(x1, y1, x2, y2,   tmp) {
  if (x1 == x2) {
    if (y1 > y2) { tmp = y1; y1 = y2; y2 = tmp; }
    for (yy = y1; yy <= y2; yy++) {
      grid[x1","yy] = 1;
      if (yy > floor_level) floor_level = yy;
    }
  } else {
    if (x1 > x2) { tmp = x1; x1 = x2; x2 = tmp; }
    for (xx = x1; xx <= x2; xx++) {
      grid[xx","y1] = 1;
      if (y1 > floor_level) floor_level = y1;
    }
  }
}
function do_fill(   startKey, sands, first_floor_touch) {
  floor_level++;
  sands = 0;
  first_floor_touch = 0;
  startKey = "500,0";
  while ((startKey in grid) != 1) {
    sx = 500; sy = 0; settled = 0;
    while (!settled) {
      downKey = sx "," (sy+1);
      leftKey = (sx-1) "," (sy+1);
      rightKey = (sx+1) "," (sy+1);
      moved = 0;
      if (!(downKey in grid)) {
        sy = sy + 1; moved = 1;
      } else if (!(leftKey in grid)) {
        sx = sx - 1; sy = sy + 1; moved = 1;
      } else if (!(rightKey in grid)) {
        sx = sx + 1; sy = sy + 1; moved = 1;
      }
      if (!moved) {
        grid[sx","sy] = 1; settled = 1;
      }
      if (sy == floor_level) {
        if (first_floor_touch == 0) first_floor_touch = sands;
        grid[sx","sy] = 1; settled = 1;
      }
    }
    sands++;
  }
  return first_floor_touch;
}