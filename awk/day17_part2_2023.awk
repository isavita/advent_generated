#!/usr/bin/awk -f
# AWK solution equivalent to the given C/Dijkstra approach
function digit(r, c) { return substr(grid[r], c+1, 1) + 0 }

function dist_get(r, c, d, s) { return dist[r, c, d, s] }
function dist_set(r, c, d, s, v) { dist[r, c, d, s] = v }

function heap_push(cost, r, c, dir, steps) {
  heap_size++
  h_cost[heap_size] = cost
  h_r[heap_size] = r
  h_c[heap_size] = c
  h_d[heap_size] = dir
  h_s[heap_size] = steps
  i = heap_size
  while (i > 1) {
    p = int((i - 1) / 2)
    if (h_cost[p] <= h_cost[i]) break
    tmp = h_cost[i]; h_cost[i] = h_cost[p]; h_cost[p] = tmp
    tmp = h_r[i];    h_r[i] = h_r[p];    h_r[p] = tmp
    tmp = h_c[i];    h_c[i] = h_c[p];    h_c[p] = tmp
    tmp = h_d[i];    h_d[i] = h_d[p];    h_d[p] = tmp
    tmp = h_s[i];    h_s[i] = h_s[p];    h_s[p] = tmp
    i = p
  }
}
function heap_pop() {
  if (heap_size < 1) { popped_cost = -1; return }
  popped_cost = h_cost[1]
  popped_r = h_r[1]
  popped_c = h_c[1]
  popped_dir = h_d[1]
  popped_steps = h_s[1]
  last = heap_size
  h_cost[1] = h_cost[last]
  h_r[1] = h_r[last]
  h_c[1] = h_c[last]
  h_d[1] = h_d[last]
  h_s[1] = h_s[last]
  delete h_cost[last]; delete h_r[last]; delete h_c[last]; delete h_d[last]; delete h_s[last]
  heap_size = last - 1
  i = 1
  while (1) {
    l = 2 * i
    r = 2 * i + 1
    smallest = i
    if (l <= heap_size && h_cost[l] < h_cost[smallest]) smallest = l
    if (r <= heap_size && h_cost[r] < h_cost[smallest]) smallest = r
    if (smallest == i) break
    tmp = h_cost[i]; h_cost[i] = h_cost[smallest]; h_cost[smallest] = tmp
    tmp = h_r[i];    h_r[i] = h_r[smallest];    h_r[smallest] = tmp
    tmp = h_c[i];    h_c[i] = h_c[smallest];    h_c[smallest] = tmp
    tmp = h_d[i];    h_d[i] = h_d[smallest];    h_d[smallest] = tmp
    tmp = h_s[i];    h_s[i] = h_s[smallest];    h_s[smallest] = tmp
    i = smallest
  }
}

function solve(min_turn, max_straight, height, width,   r, c, d, s, next_dir, next_r, next_c, new_cost, new_steps) {
  # initialize distances
  for (r0 = 0; r0 < height; r0++) {
    for (c0 = 0; c0 < width; c0++) {
      for (d0 = 0; d0 < 4; d0++) {
        for (s0 = 1; s0 <= max_straight; s0++) {
          dist_set(r0, c0, d0, s0, INF)
        }
      }
    }
  }
  heap_size = 0

  if (width > 1) {
    initial_cost = digit(0, 1)
    if (initial_cost < dist_get(0, 1, 2, 1)) {
      dist_set(0, 1, 2, 1, initial_cost)
      heap_push(initial_cost, 0, 1, 2, 1)
    }
  }
  if (height > 1) {
    initial_cost = digit(1, 0)
    if (initial_cost < dist_get(1, 0, 1, 1)) {
      dist_set(1, 0, 1, 1, initial_cost)
      heap_push(initial_cost, 1, 0, 1, 1)
    }
  }

  while (heap_size > 0) {
    heap_pop()
    cost = popped_cost
    r = popped_r
    c = popped_c
    dir = popped_dir
    s = popped_steps

    if (cost > dist_get(r, c, dir, s)) {
      continue
    }

    if (r == height - 1 && c == width - 1 && s >= min_turn) {
      return cost
    }

    for (next_dir = 0; next_dir < 4; next_dir++) {
      if ((dir == 0 && next_dir == 1) || (dir == 1 && next_dir == 0) || (dir == 2 && next_dir == 3) || (dir == 3 && next_dir == 2)) {
        continue
      }
      next_r = r + dr[next_dir]
      next_c = c + dc[next_dir]
      if (next_r < 0 || next_r >= height || next_c < 0 || next_c >= width) {
        continue
      }
      new_cost = cost + digit(next_r, next_c)
      if (next_dir == dir) {
        if (s >= max_straight) continue
        new_steps = s + 1
      } else {
        if (s < min_turn) continue
        new_steps = 1
      }
      if (new_cost < dist_get(next_r, next_c, next_dir, new_steps)) {
        dist_set(next_r, next_c, next_dir, new_steps, new_cost)
        heap_push(new_cost, next_r, next_c, next_dir, new_steps)
      }
    }
  }
  return -1
}

BEGIN {
  height = 0
  while ((getline line < "input.txt") > 0) {
    grid[height] = line
    height++
  }
  if (height <= 0) { print "Input grid is empty"; exit 1 }
  width = length(grid[0])
  for (r = 0; r < height; r++) {
    if (length(grid[r]) != width) { print "Inconsistent line width at line " (r+1); exit 1 }
  }

  dr[0] = -1; dr[1] = 1; dr[2] = 0; dr[3] = 0
  dc[0] = 0;  dc[1] = 0; dc[2] = 1; dc[3] = -1
  INF = 1e100

  res1 = solve(1, 3, height, width)
  print res1
  res2 = solve(4, 10, height, width)
  print res2
  exit
}

# grid and helper declarations
BEGIN{ # keep empty to ensure functions above are defined before use if needed
}