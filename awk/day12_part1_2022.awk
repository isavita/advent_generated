BEGIN {
  for (i = 97; i <= 122; i++) {
    ch = sprintf("%c", i);
    heightMap[ch] = i - 97;
  }
  dx[1] = 0; dy[1] = 1;
  dx[2] = 0; dy[2] = -1;
  dx[3] = 1; dy[3] = 0;
  dx[4] = -1; dy[4] = 0;
  sx = sy = ex = ey = -1;
  cols = 0; rows = 0;

  while ((getline line < "input.txt") > 0) {
    if (length(line) == 0) { next; }
    if (cols == 0) cols = length(line);
    row = rows;
    for (i = 1; i <= length(line); i++) {
      ch = substr(line, i, 1);
      grid[row","(i-1)] = ch;
      if (ch == "S") {
        sx = i - 1; sy = row;
        grid[sy","sx] = "a";
      } else if (ch == "E") {
        ex = i - 1; ey = row;
        grid[ey","ex] = "z";
      }
    }
    rows++;
  }

  head = 1; tail = 0;
  dist[ey","ex] = 0;
  qx[++tail] = ex; qy[tail] = ey;

  while (head <= tail) {
    x = qx[head]; y = qy[head]; head++;
    key = y","x; d = dist[key];
    hcur = heightMap[ grid[key] ];
    for (k = 1; k <= 4; k++) {
      nx = x + dx[k]; ny = y + dy[k];
      if (nx < 0 || ny < 0 || nx >= cols || ny >= rows) continue;
      nkey = ny","nx;
      nh = heightMap[ grid[nkey] ];
      if (hcur - nh <= 1) {
        if (dist[nkey] == "") {
          dist[nkey] = d + 1;
          qx[++tail] = nx; qy[tail] = ny;
        }
      }
    }
  }

  print dist[sy","sx];
  exit
}