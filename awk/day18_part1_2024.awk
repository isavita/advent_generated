
BEGIN {
  grid_size = 71
  max_lines = 1024
  steps = -1

  # Read corrupted coordinates from input.txt
  while (line_count < max_lines && getline line < "input.txt" > 0) {
    split(line, coords, ",")
    corrupted[coords[1] "," coords[2]] = 1
    line_count++
  }

  # Initialize queue and visited set
  q[0] = "0,0,0"
  visited["0,0"] = 1
  head = 0
  tail = 1

  # BFS
  while (head < tail) {
    curr = q[head++]
    split(curr, parts, ",")
    x = parts[1]
    y = parts[2]
    s = parts[3]

    if (x == grid_size - 1 && y == grid_size - 1) {
      steps = s
      break
    }

    # Explore neighbors
    for (i = 1; i <= 4; i++) {
      if (i == 1) { dx = 0; dy = 1 }
      else if (i == 2) { dx = 0; dy = -1 }
      else if (i == 3) { dx = 1; dy = 0 }
      else { dx = -1; dy = 0 }

      nx = x + dx
      ny = y + dy
      
      if (nx >= 0 && nx < grid_size && ny >= 0 && ny < grid_size) {
        coord_str = nx "," ny
        if (!(coord_str in corrupted) && !(coord_str in visited)) {
          q[tail++] = nx "," ny "," (s + 1)
          visited[coord_str] = 1
        }
      }
    }
  }
  print steps
}
