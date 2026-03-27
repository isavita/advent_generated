
lines <- readLines("input.txt", warn = FALSE)
if (length(lines) > 0) {
  grid <- do.call(rbind, strsplit(lines, ""))
  s_pos <- which(grid == "S", arr.ind = TRUE)
  s_pos <- s_pos[order(s_pos[, 1], s_pos[, 2]), , drop = FALSE]
  
  y <- if (nrow(s_pos) > 0) s_pos[1, 1] else 1
  active <- if (nrow(s_pos) > 0) s_pos[1, 2] else 1
  splits <- 0
  nc <- ncol(grid)
  nr <- nrow(grid)
  
  while (y <= nr && length(active) > 0) {
    is_caret <- grid[y, active] == "^"
    splits <- splits + sum(is_caret)
    
    next_active <- c(active[!is_caret], active[is_caret] - 1, active[is_caret] + 1)
    active <- unique(next_active[next_active >= 1 & next_active <= nc])
    y <- y + 1
  }
  cat(splits, "\n")
}
