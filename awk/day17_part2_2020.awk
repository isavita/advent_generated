
function simulate_cycle_4d(active_cubes,   new_active_cubes, neighbor_counts, coord, dw, dz, dy, dx, neighbor, count, split_coords, i) {
  delete neighbor_counts
  delete new_active_cubes

  for (coord in active_cubes) {
    split(coord, split_coords, SUBSEP)
    x = split_coords[1]
    y = split_coords[2]
    z = split_coords[3]
    w = split_coords[4]
    for (dw = -1; dw <= 1; dw++) {
      for (dz = -1; dz <= 1; dz++) {
        for (dy = -1; dy <= 1; dy++) {
          for (dx = -1; dx <= 1; dx++) {
            if (dw == 0 && dz == 0 && dy == 0 && dx == 0) {
              continue
            }
            neighbor = (x + dx) SUBSEP (y + dy) SUBSEP (z + dz) SUBSEP (w + dw)
            neighbor_counts[neighbor]++
          }
        }
      }
    }
  }

  for (neighbor in neighbor_counts) {
    count = neighbor_counts[neighbor]
    if (count == 3 || (count == 2 && (neighbor in active_cubes))) {
      new_active_cubes[neighbor] = 1
    }
  }

  delete active_cubes
  for(coord in new_active_cubes) {
    active_cubes[coord] = 1
  }

  return
}

function main(   initialState, line, x, y, char, cycle, active_cubes, i, len) {
  initialState = ""
  while (getline line < "input.txt" > 0) {
    if (initialState != "") {
      initialState = initialState "\n" line
    } else {
      initialState = line
    }
  }

  split(initialState, lines, "\n")
  len = length(lines)
  for (y = 1; y <= len; y++) {
    line = lines[y]
    for (x = 1; x <= length(line); x++) {
      char = substr(line, x, 1)
      if (char == "#") {
        active_cubes[x SUBSEP y SUBSEP 0 SUBSEP 0] = 1
      }
    }
  }

  for (cycle = 1; cycle <= 6; cycle++) {
    simulate_cycle_4d(active_cubes)
  }

  i = 0
  for (coord in active_cubes) {
    i++
  }
  print i
}

BEGIN {
  SUBSEP = ","
  main()
  exit
}
