simulate_sand <- function(paths) {
  cave <- list()
  for (path in paths) {
    coords <- strsplit(path, " -> ")[[1]]
    coords <- lapply(coords, function(x) as.integer(unlist(strsplit(x, ","))))
    
    for (i in seq_along(coords)[-1]) {
      start <- coords[[i - 1]]
      end <- coords[[i]]
      if (start[1] == end[1]) {
        for (y in min(start[2], end[2]):max(start[2], end[2])) {
          cave[[paste0(start[1], ",", y)]] <- "#"
        }
      } else {
        for (x in min(start[1], end[1]):max(start[1], end[1])) {
          cave[[paste0(x, ",", start[2])]] <- "#"
        }
      }
    }
  }

  sand_source <- "500,0"
  units_of_sand <- 0
  while (TRUE) {
    sand_pos <- c(500, 0)
    while (TRUE) {
      below <- paste0(sand_pos[1], ",", sand_pos[2] + 1)
      left_diag <- paste0(sand_pos[1] - 1, ",", sand_pos[2] + 1)
      right_diag <- paste0(sand_pos[1] + 1, ",", sand_pos[2] + 1)

      if (!below %in% names(cave)) {
        sand_pos[2] <- sand_pos[2] + 1
      } else if (!left_diag %in% names(cave)) {
        sand_pos <- c(sand_pos[1] - 1, sand_pos[2] + 1)
      } else if (!right_diag %in% names(cave)) {
        sand_pos <- c(sand_pos[1] + 1, sand_pos[2] + 1)
      } else {
        cave[[paste0(sand_pos[1], ",", sand_pos[2])]] <- "o"
        units_of_sand <- units_of_sand + 1
        break
      }

      if (sand_pos[2] > 1000) return(units_of_sand)  # Abyss condition
    }
  }
}

main <- function() {
  lines <- readLines("input.txt")
  result <- simulate_sand(lines)
  cat(result, "\n")
}

main()