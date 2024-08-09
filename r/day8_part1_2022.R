grid <- list()
visible <- list()
neighbors4 <- list(c(0, 1), c(0, -1), c(1, 0), c(-1, 0))

lines <- readLines("input.txt", warn = FALSE)
for (y in seq_along(lines)) {
  for (x in seq_along(strsplit(lines[y], "")[[1]])) {
    grid[[paste(x - 1, y - 1, sep = ",")]] <- as.integer(substr(lines[y], x, x))
  }
}

for (p in names(grid)) {
  p_coords <- as.numeric(unlist(strsplit(p, ",")))
  visible_flag <- FALSE
  
  for (n in neighbors4) {
    next_coords <- p_coords
    repeat {
      next_coords <- next_coords + n
      next_key <- paste(next_coords[1], next_coords[2], sep = ",")
      if (next_key %in% names(grid)) {
        if (grid[[next_key]] >= grid[[p]]) break
      } else {
        visible_flag <- TRUE
        break
      }
    }
    if (visible_flag) {
      visible[[p]] <- TRUE
      break
    }
  }
}

cat(length(visible), "\n")