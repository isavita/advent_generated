#!/usr/bin/awk -f
BEGIN {
  EMPTY = "."
  ROUND_ROCK = "O"
}
function idx(x,y) { return y*W + x }
function cell(x,y) { return G[idx(x,y)] }
function setCell(x,y,v) { G[idx(x,y)] = v }
function isInBounds(x,y) { return x >= 0 && y >= 0 && x < W && y < H }
function shiftSingleRock(x,y,dx,dy) {
  if (cell(x,y) == ROUND_ROCK) {
    nx = x + dx; ny = y + dy
    while (isInBounds(nx, ny) && cell(nx,ny) == "") {
      setCell(nx, ny, ROUND_ROCK)
      setCell(x, y, "")
      x = nx; y = ny
      nx = x + dx; ny = y + dy
    }
  }
}
function shiftRocks(dx, dy) {
  if (dy < 0 || dx < 0) {
    for (x = 0; x < W; x++) {
      for (y = 0; y < H; y++) {
        shiftSingleRock(x,y,dx,dy)
      }
    }
  } else {
    for (x = W-1; x >= 0; x--) {
      for (y = H-1; y >= 0; y--) {
        shiftSingleRock(x,y,dx,dy)
      }
    }
  }
}
function cycleRocks() { shiftRocks(0,-1); shiftRocks(-1,0); shiftRocks(0,1); shiftRocks(1,0) }
function gridKey() {
  key = 0
  for (x=0; x<W; x++) {
    for (y=0; y<H; y++) {
      if (cell(x,y) == ROUND_ROCK) key += x + y*W
    }
  }
  return key
}
function calculateLoad() {
  load = 0
  for (x=0; x<W; x++) {
    for (y=0; y<H; y++) {
      if (cell(x,y) == ROUND_ROCK) load += H - y
    }
  }
  return load
}
END {
  n = 0
  while ((getline line < "input.txt") > 0) {
    lines[++n] = line
  }
  if (n == 0) { print 0; exit }
  W = length(lines[1]); H = n
  delete G
  for (y = 0; y < H; y++) {
    line = lines[y+1]
    for (x = 0; x < W; x++) {
      ch = substr(line, x+1, 1)
      if (ch != EMPTY) G[y*W + x] = ch
    }
  }
  delete CACHE
  numCycles = 1000000000
  for (i = 0; i < numCycles; i++) {
    key = gridKey()
    if (key in CACHE) {
      prev = CACHE[key]
      cycleLen = i - prev
      remaining = (numCycles - prev) % cycleLen
      for (t = 0; t < remaining; t++) cycleRocks()
      print calculateLoad()
      exit
    }
    CACHE[key] = i
    cycleRocks()
  }
  print calculateLoad()
}