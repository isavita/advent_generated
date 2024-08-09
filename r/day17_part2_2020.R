read_input <- function(file) {
  lines <- readLines(file)
  active_cubes <- list()
  for (y in seq_along(lines)) {
    for (x in seq_along(strsplit(lines[y], "")[[1]])) {
      if (substr(lines[y], x, x) == "#") {
        active_cubes[[paste(x - 1, y - 1, 0, 0, sep = ",")]] <- TRUE
      }
    }
  }
  return(active_cubes)
}

simulate_cycle_4d <- function(active_cubes) {
  new_active_cubes <- list()
  neighbor_counts <- table(unlist(lapply(names(active_cubes), function(coord) {
    coords <- as.integer(strsplit(coord, ",")[[1]])
    neighbors <- expand.grid(
      x = (coords[1] - 1):(coords[1] + 1),
      y = (coords[2] - 1):(coords[2] + 1),
      z = (coords[3] - 1):(coords[3] + 1),
      w = (coords[4] - 1):(coords[4] + 1)
    )
    neighbors <- neighbors[!(neighbors$x == coords[1] & neighbors$y == coords[2] & neighbors$z == coords[3] & neighbors$w == coords[4]), ]
    apply(neighbors, 1, function(n) paste(n, collapse = ","))
  })))
  
  for (coord in names(neighbor_counts)) {
    count <- neighbor_counts[coord]
    if (count == 3 || (count == 2 && coord %in% names(active_cubes))) {
      new_active_cubes[[coord]] <- TRUE
    }
  }
  
  return(new_active_cubes)
}

main <- function() {
  active_cubes <- read_input("input.txt")
  for (cycle in 1:6) {
    active_cubes <- simulate_cycle_4d(active_cubes)
  }
  cat(length(active_cubes), "\n")
}

main()