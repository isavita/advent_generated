#!/usr/bin/awk -f
BEGIN {
  DR[1] = 1; DR[2] = -1; DR[3] = 0; DR[4] = 0
  DC[1] = 0; DC[2] = 0; DC[3] = 1; DC[4] = -1
  H = 0
  while ((getline line < "input.txt") > 0) {
    H++
    grid[H] = line
  }
  if (H == 0) { print 0; exit }
  W = length(grid[1])

  Ntrack = 0
  for (r = 1; r <= H; r++) {
    line = grid[r]
    plen = length(line)
    for (c = 1; c <= plen; c++) {
      ch = substr(line, c, 1)
      if (ch == "#") {
        walls[r, c] = 1
      } else {
        track_cells[++Ntrack] = r "," c
        if (ch == "S") { S_r = r; S_c = c }
        else if (ch == "E") { E_r = r; E_c = c }
      }
    }
  }
  if (S_r == "" || E_r == "" ) { print 0; exit }

  bfs_S()
  bfs_E()
  ds = distS[E_r, E_c]
  if (ds == "") { print 0; exit }
  normal_cost = ds + 0

  for (i = 1; i <= Ntrack; i++) {
    split(track_cells[i], arr, ",")
    sr = arr[1]; sc = arr[2]
    sd = distS[sr, sc]
    if (sd == "") continue
    sd = sd + 0

    delete distC
    qhead = 1; qtail = 0
    distC[sr, sc] = 0
    q[++qtail] = sr; qc[qtail] = sc
    while (qhead <= qtail) {
      r = q[qhead]; c = qc[qhead]; qhead++
      step = distC[r, c]
      if (step >= 20) continue
      for (dir = 1; dir <= 4; dir++) {
        nr = r + DR[dir]; nc = c + DC[dir]
        if (nr >= 1 && nr <= H && nc >= 1 && nc <= W) {
          if (distC[nr, nc] == "") {
            distC[nr, nc] = step + 1
            q[++qtail] = nr; qc[qtail] = nc
          }
        }
      }
    }
    for (rr = 1; rr <= H; rr++) {
      for (cc = 1; cc <= W; cc++) {
        s = distC[rr, cc]
        if (s != "" && s > 0 && s <= 20 && walls[rr, cc] != 1) {
          ed = distE[rr, cc]
          if (ed == "") continue
          cost = sd + s + ed
          if (cost < normal_cost) {
            key = sr "," sc "," rr "," cc
            if (cheats[key] == "" || cost < cheats[key]) cheats[key] = cost
          }
        }
      }
    }
  }

  count = 0
  for (k in cheats) {
    cost = cheats[k] + 0
    if (normal_cost - cost >= 100) count++
  }
  print count
  exit
}

function bfs_S() {
  delete distS
  qhead = 1; qtail = 0
  distS[S_r, S_c] = 0
  q[++qtail] = S_r; qc[qtail] = S_c
  while (qhead <= qtail) {
    r = q[qhead]; c = qc[qhead]; qhead++
    d = distS[r, c] + 0
    for (dir = 1; dir <= 4; dir++) {
      nr = r + DR[dir]; nc = c + DC[dir]
      if (nr >= 1 && nr <= H && nc >= 1 && nc <= W) {
        if (walls[nr, nc] != 1 && distS[nr, nc] == "") {
          distS[nr, nc] = d + 1
          q[++qtail] = nr; qc[qtail] = nc
        }
      }
    }
  }
}
function bfs_E() {
  delete distE
  qhead = 1; qtail = 0
  distE[E_r, E_c] = 0
  q[++qtail] = E_r; qc[qtail] = E_c
  while (qhead <= qtail) {
    r = q[qhead]; c = qc[qhead]; qhead++
    d = distE[r, c] + 0
    for (dir = 1; dir <= 4; dir++) {
      nr = r + DR[dir]; nc = c + DC[dir]
      if (nr >= 1 && nr <= H && nc >= 1 && nc <= W) {
        if (walls[nr, nc] != 1 && distE[nr, nc] == "") {
          distE[nr, nc] = d + 1
          q[++qtail] = nr; qc[qtail] = nc
        }
      }
    }
  }
}