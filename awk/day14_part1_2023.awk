
#!/usr/bin/awk -f

function shift_single_rock(x, y, dx, dy,  nx, ny) {
  if (grid[x,y] == "O") {
    nx = x + dx
    ny = y + dy
    while (nx >= 0 && nx < width && ny >= 0 && ny < height && !grid[nx,ny]) {
      grid[nx,ny] = "O"
      grid[x,y] = ""
      x = nx
      y = ny
      nx = x + dx
      ny = y + dy
    }
  }
}

function shift_rocks(dx, dy,  x, y) {
  if (dx == 0 && dy == -1) {
    for (x = 0; x < width; x++) {
      for (y = 0; y < height; y++) {
        shift_single_rock(x, y, dx, dy)
      }
    }
  } else if (dx == -1 && dy == 0) {
    for (x = 0; x < width; x++) {
      for (y = 0; y < height; y++) {
        shift_single_rock(x, y, dx, dy)
      }
    }
  } else {
    for (x = width - 1; x >= 0; x--) {
      for (y = height - 1; y >= 0; y--) {
        shift_single_rock(x, y, dx, dy)
      }
    }
  }
}

function calculate_load(  x, y, load) {
  load = 0
  for (x = 0; x < width; x++) {
    for (y = 0; y < height; y++) {
      if (grid[x,y] == "O") {
        load += height - y
      }
    }
  }
  return load
}

function solve(  i, j) {
  width = length(input[1])
  height = length(input)
  for (i = 1; i <= height; i++) {
    line = input[i]
    for (j = 1; j <= width; j++) {
      cell = substr(line, j, 1)
      if (cell != ".") {
        grid[j-1, i-1] = cell
      } else {
        grid[j-1, i-1] = ""
      }
    }
  }
  shift_rocks(0, -1)
  print calculate_load()
}

BEGIN {
  i = 1
  while (getline line < "input.txt" > 0) {
    input[i++] = line
  }
  solve()
  close("input.txt")
  exit
}
