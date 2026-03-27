
lines <- readLines("input.txt")
mat <- do.call(rbind, strsplit(lines, ""))
pos <- which(mat == "#", arr.ind = TRUE)
n_elves <- nrow(pos)
grid_size <- 2000
off <- 800
grid <- matrix(0L, grid_size, grid_size)
pos <- pos + off
grid[pos] <- 1L
order <- 1:4
round <- 0
move_dr <- c(-1, 1, 0, 0)
move_dc <- c(0, 0, -1, 1)

repeat {
  round <- round + 1
  r <- pos[, 1]
  c <- pos[, 2]
  
  has_nb <- (grid[cbind(r - 1, c - 1)] | grid[cbind(r - 1, c)] | grid[cbind(r - 1, c + 1)] |
             grid[cbind(r,     c - 1)] |                       grid[cbind(r,     c + 1)] |
             grid[cbind(r + 1, c - 1)] | grid[cbind(r + 1, c)] | grid[cbind(r + 1, c + 1)]) == 1L
  
  someone_moved <- FALSE
  if (any(has_nb)) {
    prop_r <- rep(NA_integer_, n_elves)
    prop_c <- rep(NA_integer_, n_elves)
    still_looking <- has_nb
    
    for (d in order) {
      if (!any(still_looking)) break
      check <- if (d == 1) {
        (grid[cbind(r - 1, c - 1)] | grid[cbind(r - 1, c)] | grid[cbind(r - 1, c + 1)]) == 0L
      } else if (d == 2) {
        (grid[cbind(r + 1, c - 1)] | grid[cbind(r + 1, c)] | grid[cbind(r + 1, c + 1)]) == 0L
      } else if (d == 3) {
        (grid[cbind(r - 1, c - 1)] | grid[cbind(r,     c - 1)] | grid[cbind(r + 1, c - 1)]) == 0L
      } else {
        (grid[cbind(r - 1, c + 1)] | grid[cbind(r,     c + 1)] | grid[cbind(r + 1, c + 1)]) == 0L
      }
      can_go <- still_looking & check
      prop_r[can_go] <- r[can_go] + move_dr[d]
      prop_c[can_go] <- c[can_go] + move_dc[d]
      still_looking[can_go] <- FALSE
    }
    
    prop_h <- prop_r * 3000L + prop_c
    move_idx <- which(!is.na(prop_h))
    
    if (length(move_idx) > 0) {
      h_vals <- prop_h[move_idx]
      dups <- duplicated(h_vals) | duplicated(h_vals, fromLast = TRUE)
      final_idx <- move_idx[!dups]
      
      if (length(final_idx) > 0) {
        grid[pos[final_idx, , drop = FALSE]] <- 0L
        pos[final_idx, 1] <- prop_r[final_idx]
        pos[final_idx, 2] <- prop_c[final_idx]
        grid[pos[final_idx, , drop = FALSE]] <- 1L
        someone_moved <- TRUE
      }
    }
  }
  
  if (!someone_moved) {
    cat(round, "\n")
    break
  }
  order <- c(order[-1], order[1])
}
