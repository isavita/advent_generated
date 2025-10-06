BEGIN {
  minx = 1e9; miny = 1e9; minz = 1e9;
  maxx = -1e9; maxy = -1e9; maxz = -1e9;
  while ((getline line < "input.txt") > 0) {
    if (line ~ /^[[:space:]]*$/) continue;
    n = split(line, p, ",");
    if (n == 3) {
      x = p[1] + 0; y = p[2] + 0; z = p[3] + 0;
      key = x "," y "," z;
      cubes[key] = 1;
      if (x < minx) minx = x;
      if (y < miny) miny = y;
      if (z < minz) minz = z;
      if (x > maxx) maxx = x;
      if (y > maxy) maxy = y;
      if (z > maxz) maxz = z;
    }
  }
  close("input.txt");
  minx--; miny--; minz--;
  maxx++; maxy++; maxz++;

  head = 0; tail = 0;
  qx[tail] = minx; qy[tail] = miny; qz[tail] = minz; tail++;

  visited[minx "," miny "," minz] = 1;

  dx[0] = -1; dy[0] = 0; dz[0] = 0;
  dx[1] = 1; dy[1] = 0; dz[1] = 0;
  dx[2] = 0; dy[2] = -1; dz[2] = 0;
  dx[3] = 0; dy[3] = 1; dz[3] = 0;
  dx[4] = 0; dy[4] = 0; dz[4] = -1;
  dx[5] = 0; dy[5] = 0; dz[5] = 1;
}

END {
  face = 0;
  while (head < tail) {
    cx = qx[head]; cy = qy[head]; cz = qz[head]; head++;
    for (d = 0; d < 6; d++) {
      nx = cx + dx[d]; ny = cy + dy[d]; nz = cz + dz[d];
      if (nx < minx || ny < miny || nz < minz || nx > maxx || ny > maxy || nz > maxz) continue;
      nk = nx "," ny "," nz;
      if (nk in cubes) {
        face++;
      } else if (!(nk in visited)) {
        visited[nk] = 1;
        qx[tail] = nx; qy[tail] = ny; qz[tail] = nz; tail++;
      }
    }
  }
  print face;
}